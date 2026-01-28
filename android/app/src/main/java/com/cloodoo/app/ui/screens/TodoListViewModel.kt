package com.cloodoo.app.ui.screens

import android.app.Application
import android.util.Log
import androidx.lifecycle.AndroidViewModel
import androidx.lifecycle.ViewModel
import androidx.lifecycle.ViewModelProvider
import androidx.lifecycle.viewModelScope
import com.cloodoo.app.data.local.CloodooDatabase
import com.cloodoo.app.data.local.TodoEntity
import com.cloodoo.app.data.remote.ConnectionState
import com.cloodoo.app.data.remote.SyncEvent
import com.cloodoo.app.data.remote.SyncManager
import com.cloodoo.app.data.repository.TodoRepository
import com.cloodoo.app.data.security.CertificateManager
import com.cloodoo.app.ui.components.DateGroup
import com.cloodoo.app.ui.components.TodoGroupData
import com.cloodoo.app.ui.components.groupTodosByDate
import kotlinx.coroutines.flow.*
import kotlinx.coroutines.launch
import java.time.ZonedDateTime
import java.time.format.DateTimeFormatter

class TodoListViewModel(
    application: Application,
    private val certificateManager: CertificateManager
) : AndroidViewModel(application) {

    companion object {
        private const val TAG = "TodoListViewModel"
    }

    private val deviceId = certificateManager.getDeviceName() ?: "unknown"
    private val database = CloodooDatabase.getDatabase(application)
    private val repository = TodoRepository(database, deviceId)
    private val syncManager = SyncManager(database, certificateManager, deviceId)

    // UI State
    private val _uiState = MutableStateFlow(TodoListUiState())
    val uiState: StateFlow<TodoListUiState> = _uiState.asStateFlow()

    // Track which sections are collapsed (Completed starts collapsed)
    private val _collapsedSections = MutableStateFlow(setOf(DateGroup.COMPLETED))

    // Connection state from SyncManager
    val connectionState: StateFlow<ConnectionState> = syncManager.connectionState

    private val _lastSyncTime = MutableStateFlow<String?>(null)

    init {
        // Load last sync time from preferences
        loadLastSyncTime(application)

        // Observe current TODOs and compute grouped data
        viewModelScope.launch {
            combine(
                repository.getCurrentTodos(),
                _collapsedSections
            ) { todos, collapsed ->
                val grouped = groupTodosByDate(todos).map { groupData ->
                    groupData.copy(isExpanded = groupData.group !in collapsed)
                }
                Pair(todos, grouped)
            }.collect { (todos, grouped) ->
                _uiState.update { it.copy(todos = todos, groupedTodos = grouped, isLoading = false) }
            }
        }

        // Observe connection state
        viewModelScope.launch {
            syncManager.connectionState.collect { state ->
                when (state) {
                    ConnectionState.CONNECTED -> {
                        _uiState.update { it.copy(isSyncing = false, lastSyncResult = "Connected") }
                    }
                    ConnectionState.CONNECTING -> {
                        _uiState.update { it.copy(isSyncing = true, syncError = null) }
                    }
                    ConnectionState.ERROR -> {
                        _uiState.update { it.copy(isSyncing = false, syncError = "Connection error") }
                    }
                    ConnectionState.DISCONNECTED -> {
                        _uiState.update { it.copy(isSyncing = false) }
                    }
                }
            }
        }

        // Observe sync events
        viewModelScope.launch {
            syncManager.syncEvents.collect { event ->
                when (event) {
                    is SyncEvent.Connected -> {
                        _lastSyncTime.value = event.serverTime
                        saveLastSyncTime(getApplication())
                        _uiState.update {
                            it.copy(
                                isSyncing = false,
                                lastSyncResult = if (event.pendingChanges > 0)
                                    "Receiving ${event.pendingChanges} changes..."
                                else
                                    "Connected - up to date"
                            )
                        }
                    }
                    is SyncEvent.Received -> {
                        Log.d(TAG, "Received ${event.type} for ${event.todoId}")
                    }
                    is SyncEvent.Sent -> {
                        Log.d(TAG, "Sent change for ${event.todoId}")
                    }
                    is SyncEvent.Error -> {
                        _uiState.update { it.copy(syncError = event.message) }
                    }
                }
            }
        }

        // Auto-connect on startup
        connect()
    }

    fun connect() {
        viewModelScope.launch {
            val savedSyncTime = _lastSyncTime.value

            // If we have a saved sync time but no local data, force a full sync
            val since = if (savedSyncTime != null && repository.isEmpty()) {
                Log.d(TAG, "Local DB empty despite saved sync time, forcing full sync")
                _lastSyncTime.value = null
                saveLastSyncTime(getApplication())
                null
            } else {
                savedSyncTime
            }
            syncManager.connect(since, viewModelScope)
        }
    }

    fun disconnect() {
        syncManager.disconnect()
    }

    fun refreshSync() {
        disconnect()
        // Clear sync time to force full resync
        _lastSyncTime.value = null
        saveLastSyncTime(getApplication())
        connect()
    }

    fun unpair() {
        disconnect()
        certificateManager.removeCertificate()
    }

    fun createTodo(
        title: String,
        description: String? = null,
        priority: String = "medium",
        dueDate: String? = null,
        scheduledDate: String? = null,
        tags: String? = null,
        repeatInterval: Int? = null,
        repeatUnit: String? = null
    ) {
        if (title.isBlank()) return

        viewModelScope.launch {
            val todo = repository.createTodo(
                title = title.trim(),
                description = description,
                priority = priority,
                dueDate = dueDate,
                scheduledDate = scheduledDate,
                tags = tags,
                repeatInterval = repeatInterval,
                repeatUnit = repeatUnit
            )
            syncManager.sendTodoUpsert(todo)
        }
    }

    fun toggleComplete(todoId: String) {
        viewModelScope.launch {
            val todo = _uiState.value.todos.find { it.id == todoId }
            if (todo != null) {
                val isDone = todo.status == "completed" || todo.status == "cancelled"
                if (isDone) {
                    repository.updateTodo(todoId, status = "pending", completedAt = "")
                } else {
                    // Check if this is a recurring task
                    val interval = todo.repeatInterval
                    val unit = todo.repeatUnit
                    if (interval != null && interval > 0 && !unit.isNullOrEmpty()) {
                        // Reschedule instead of completing
                        val newScheduled = shiftDate(todo.scheduledDate, interval, unit)
                        val newDue = shiftDate(todo.dueDate, interval, unit)
                        repository.updateTodo(
                            todoId,
                            status = "pending",
                            completedAt = "",
                            scheduledDate = newScheduled ?: todo.scheduledDate,
                            dueDate = newDue ?: todo.dueDate
                        )
                    } else {
                        repository.completeTodo(todoId)
                        _uiState.update { it.copy(showConfetti = true) }
                    }
                }
                val updated = database.todoDao().getCurrentById(todoId)
                if (updated != null) {
                    syncManager.sendTodoUpsert(updated)
                }
            }
        }
    }

    private fun shiftDate(isoDate: String?, interval: Int, unit: String): String? {
        if (isoDate.isNullOrEmpty()) return null
        return try {
            val zdt = ZonedDateTime.parse(isoDate)
            val shifted = when (unit) {
                "day" -> zdt.plusDays(interval.toLong())
                "week" -> zdt.plusWeeks(interval.toLong())
                "month" -> zdt.plusMonths(interval.toLong())
                "year" -> zdt.plusYears(interval.toLong())
                else -> return null
            }
            shifted.format(DateTimeFormatter.ISO_OFFSET_DATE_TIME)
        } catch (e: Exception) {
            null
        }
    }

    fun cancelTodo(todoId: String) {
        viewModelScope.launch {
            repository.updateTodo(todoId, status = "cancelled")
            val updated = database.todoDao().getCurrentById(todoId)
            if (updated != null) {
                syncManager.sendTodoUpsert(updated)
            }
        }
    }

    fun clearConfetti() {
        _uiState.update { it.copy(showConfetti = false) }
    }

    fun deleteTodo(todoId: String) {
        viewModelScope.launch {
            repository.deleteTodo(todoId)
            syncManager.sendTodoDelete(todoId)
        }
    }

    fun toggleSectionExpanded(group: DateGroup) {
        _collapsedSections.update { collapsed ->
            if (group in collapsed) {
                collapsed - group
            } else {
                collapsed + group
            }
        }
    }

    fun updateTodo(todoId: String, dueDate: String? = null, scheduledDate: String? = null, tags: String? = null) {
        viewModelScope.launch {
            repository.updateTodo(todoId, dueDate = dueDate, scheduledDate = scheduledDate, tags = tags)
            val updated = database.todoDao().getCurrentById(todoId)
            if (updated != null) {
                syncManager.sendTodoUpsert(updated)
            }
        }
    }

    fun clearSyncMessage() {
        _uiState.update { it.copy(lastSyncResult = null, syncError = null) }
    }

    override fun onCleared() {
        super.onCleared()
        disconnect()
    }

    private fun loadLastSyncTime(application: Application) {
        val prefs = application.getSharedPreferences("cloodoo_prefs", 0)
        _lastSyncTime.value = prefs.getString("last_sync_time", null)
    }

    private fun saveLastSyncTime(application: Application) {
        val prefs = application.getSharedPreferences("cloodoo_prefs", 0)
        prefs.edit()
            .putString("last_sync_time", _lastSyncTime.value)
            .apply()
    }

    class Factory(
        private val application: Application,
        private val certificateManager: CertificateManager
    ) : ViewModelProvider.Factory {
        @Suppress("UNCHECKED_CAST")
        override fun <T : ViewModel> create(modelClass: Class<T>): T {
            if (modelClass.isAssignableFrom(TodoListViewModel::class.java)) {
                return TodoListViewModel(application, certificateManager) as T
            }
            throw IllegalArgumentException("Unknown ViewModel class")
        }
    }
}

data class TodoListUiState(
    val todos: List<TodoEntity> = emptyList(),
    val groupedTodos: List<TodoGroupData> = emptyList(),
    val isLoading: Boolean = true,
    val isSyncing: Boolean = false,
    val lastSyncResult: String? = null,
    val syncError: String? = null,
    val showConfetti: Boolean = false
)
