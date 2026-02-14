// TodoListViewModel.kt
//
// SPDX-License-Identifier: MIT
//
// Copyright (C) 2026 Anthony Green <green@moxielogic.com>

package com.cloodoo.app.ui.screens

import android.app.Application
import android.util.Log
import androidx.lifecycle.AndroidViewModel
import androidx.lifecycle.ViewModel
import androidx.lifecycle.ViewModelProvider
import androidx.lifecycle.viewModelScope
import com.cloodoo.app.data.local.AppSettingsEntity
import com.cloodoo.app.data.local.CloodooDatabase
import com.cloodoo.app.data.local.ListDefinitionEntity
import com.cloodoo.app.data.local.ListItemEntity
import com.cloodoo.app.data.local.TodoEntity
import com.cloodoo.app.data.remote.AttachmentSyncManager
import com.cloodoo.app.data.remote.ConnectionState
import com.cloodoo.app.data.remote.SyncEvent
import com.cloodoo.app.data.remote.SyncManager
import com.cloodoo.app.data.repository.AttachmentRepository
import com.cloodoo.app.data.repository.ListRepository
import com.cloodoo.app.data.repository.TodoRepository
import com.cloodoo.app.ui.components.LocalAttachment
import org.json.JSONArray
import com.cloodoo.app.data.security.CertificateManager
import com.cloodoo.app.ui.components.DateGroup
import com.cloodoo.app.ui.components.PostponeOption
import com.cloodoo.app.ui.components.TodoGroupData
import com.cloodoo.app.ui.components.groupTodosByDate
import kotlinx.coroutines.flow.*
import kotlinx.coroutines.launch
import kotlinx.coroutines.withContext
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
    private val attachmentRepository = AttachmentRepository(application, database.attachmentDao())
    private val listRepository = ListRepository(database, deviceId)
    private val syncManager = SyncManager(database, certificateManager, deviceId)

    // UI State
    private val _uiState = MutableStateFlow(TodoListUiState())
    val uiState: StateFlow<TodoListUiState> = _uiState.asStateFlow()

    // List UI State
    private val _listUiState = MutableStateFlow(ListUiState())
    val listUiState: StateFlow<ListUiState> = _listUiState.asStateFlow()

    // Track which sections are collapsed (all groups start collapsed)
    private val _collapsedSections = MutableStateFlow(DateGroup.values().toSet())

    // Connection state from SyncManager
    val connectionState: StateFlow<ConnectionState> = syncManager.connectionState

    private val _lastSyncTime = MutableStateFlow<String?>(null)

    init {
        // Load last sync time from preferences (on background thread)
        viewModelScope.launch {
            loadLastSyncTime(application)
        }

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

        // Observe list definitions
        viewModelScope.launch {
            listRepository.getListDefinitions().collect { lists ->
                _listUiState.update { it.copy(lists = lists, isLoading = false) }
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
                        // Only save lastSyncTime when there are no pending changes
                        // If there are pending changes, wait until they're all received
                        if (event.pendingChanges == 0) {
                            _lastSyncTime.value = event.serverTime
                            saveLastSyncTime(getApplication())
                        }
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
                    is SyncEvent.InitialSyncComplete -> {
                        // All pending changes received - now safe to save server time
                        _lastSyncTime.value = event.serverTime
                        saveLastSyncTime(getApplication())
                        Log.d(TAG, "Initial sync complete: ${event.changesReceived} changes received")
                        _uiState.update {
                            it.copy(lastSyncResult = "Synced ${event.changesReceived} items")
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
                    is SyncEvent.ResetRequested -> {
                        Log.i(TAG, "Sync reset requested, clearing persisted sync time")
                        // Clear or update persisted sync time
                        _lastSyncTime.value = event.resetTo
                        saveLastSyncTime(getApplication())
                        _uiState.update {
                            it.copy(
                                lastSyncResult = if (event.resetTo == null)
                                    "Full resync requested"
                                else
                                    "Partial resync from ${event.resetTo}"
                            )
                        }
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
        viewModelScope.launch {
            saveLastSyncTime(getApplication())
        }
        connect()
    }

    fun unpair() {
        disconnect()
        certificateManager.removeCertificate()
        // Clear sync timestamp to force full sync on next pairing
        _lastSyncTime.value = null
        viewModelScope.launch {
            saveLastSyncTime(getApplication())
        }
    }

    fun createTodo(
        title: String,
        description: String? = null,
        priority: String = "medium",
        dueDate: String? = null,
        scheduledDate: String? = null,
        tags: String? = null,
        repeatInterval: Int? = null,
        repeatUnit: String? = null,
        attachments: List<LocalAttachment>? = null
    ) {
        if (title.isBlank()) return

        viewModelScope.launch {
            // Register pre-stored attachments in the database
            val attachmentHashes = attachments?.map { attachment ->
                // File is already stored in app storage by PhotoAttachmentPicker
                // Just register it in the database for sync tracking
                attachmentRepository.registerExisting(
                    hash = attachment.hash,
                    filename = attachment.filename,
                    mimeType = attachment.mimeType,
                    size = attachment.size,
                    localPath = attachment.localPath
                )
                attachment.hash
            }

            // Convert hashes to JSON array string for storage
            val attachmentHashesJson = if (!attachmentHashes.isNullOrEmpty()) {
                JSONArray(attachmentHashes).toString()
            } else null

            val todo = repository.createTodo(
                title = title.trim(),
                description = description,
                priority = priority,
                dueDate = dueDate,
                scheduledDate = scheduledDate,
                tags = tags,
                repeatInterval = repeatInterval,
                repeatUnit = repeatUnit,
                attachmentHashes = attachmentHashesJson,
                enrichingP = true
            )
            syncManager.sendTodoUpsert(todo)

            // TODO: Upload attachments to server (via AttachmentSyncManager)
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

    fun postponeTodo(todoId: String, option: PostponeOption) {
        viewModelScope.launch {
            // Verify the todo exists before postponing
            _uiState.value.todos.find { it.id == todoId } ?: return@launch

            // Calculate new date based on the postpone option
            // Always use today as the base, not the task's current date
            // (so "Tomorrow" means tomorrow, not "current date + 1 day")
            val newDate = option.calculateNewDate()
            val newDateStr = newDate.atStartOfDay(java.time.ZoneId.systemDefault())
                .format(DateTimeFormatter.ISO_OFFSET_DATE_TIME)

            // Update scheduled date (primary for task scheduling)
            repository.updateTodo(todoId, scheduledDate = newDateStr)
            val updated = database.todoDao().getCurrentById(todoId)
            if (updated != null) {
                syncManager.sendTodoUpsert(updated)
            }

            Log.d(TAG, "Postponed task $todoId to $newDate using ${option.name}")
        }
    }

    fun editTodo(
        todoId: String,
        title: String,
        description: String? = null,
        priority: String,
        dueDate: String? = null,
        scheduledDate: String? = null,
        tags: String? = null,
        repeatInterval: Int? = null,
        repeatUnit: String? = null
    ) {
        if (title.isBlank()) return

        viewModelScope.launch {
            repository.updateTodo(
                todoId = todoId,
                title = title.trim(),
                description = description,
                priority = priority,
                dueDate = dueDate,
                scheduledDate = scheduledDate,
                tags = tags,
                repeatInterval = repeatInterval,
                repeatUnit = repeatUnit
            )
            val updated = database.todoDao().getCurrentById(todoId)
            if (updated != null) {
                syncManager.sendTodoUpsert(updated)
            }
        }
    }

    // ── List management methods ──

    fun getListItems(listId: String): Flow<List<ListItemEntity>> =
        listRepository.getListItems(listId)

    fun createList(name: String, description: String?, sections: String?) {
        if (name.isBlank()) return
        viewModelScope.launch {
            val entity = listRepository.createListDefinition(
                name = name.trim(),
                description = description,
                sections = sections
            )
            syncManager.sendListDefinitionUpsert(entity)
        }
    }

    fun updateList(id: String, name: String?, description: String?, sections: String?) {
        viewModelScope.launch {
            val updated = listRepository.updateListDefinition(id, name, description, sections)
            if (updated != null) {
                syncManager.sendListDefinitionUpsert(updated)
            }
        }
    }

    fun deleteList(id: String) {
        viewModelScope.launch {
            listRepository.deleteListDefinition(id)
            syncManager.sendListDefinitionDelete(id)
        }
    }

    fun addListItem(listId: String, title: String, section: String?, notes: String?) {
        if (title.isBlank()) return
        viewModelScope.launch {
            val entity = listRepository.addListItem(listId, title.trim(), section, notes)
            syncManager.sendListItemUpsert(entity)
        }
    }

    fun toggleListItem(itemId: String) {
        viewModelScope.launch {
            val updated = listRepository.toggleListItem(itemId)
            if (updated != null) {
                syncManager.sendListItemUpsert(updated)
            }
        }
    }

    fun deleteListItem(itemId: String) {
        viewModelScope.launch {
            listRepository.deleteListItem(itemId)
            syncManager.sendListItemDelete(itemId)
        }
    }

    fun deleteCheckedListItems(listId: String) {
        viewModelScope.launch {
            val items = listRepository.getCheckedListItems(listId)
            items.forEach { item ->
                listRepository.deleteListItem(item.id)
                syncManager.sendListItemDelete(item.id)
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

    private suspend fun loadLastSyncTime(application: Application) {
        withContext(kotlinx.coroutines.Dispatchers.IO) {
            val prefs = application.getSharedPreferences("cloodoo_prefs", 0)
            _lastSyncTime.value = prefs.getString("last_sync_time", null)
        }
    }

    private suspend fun saveLastSyncTime(application: Application) {
        withContext(kotlinx.coroutines.Dispatchers.IO) {
            val prefs = application.getSharedPreferences("cloodoo_prefs", 0)
            prefs.edit()
                .putString("last_sync_time", _lastSyncTime.value)
                .commit()  // Use commit() instead of apply() to ensure write completes
        }
    }

    // User Context (Enrichment) methods
    private val _userContext = MutableStateFlow("")
    val userContext: StateFlow<String> = _userContext.asStateFlow()

    init {
        // Load user context from database
        viewModelScope.launch {
            database.appSettingsDao().getSettingFlow("user_context").collect { setting ->
                _userContext.value = setting?.value ?: ""
            }
        }
    }

    fun saveUserContext(context: String) {
        viewModelScope.launch {
            val now = DateTimeFormatter.ISO_INSTANT.format(ZonedDateTime.now().toInstant())
            val setting = AppSettingsEntity(
                key = "user_context",
                value = context,
                updatedAt = now
            )
            database.appSettingsDao().insertSetting(setting)

            // Broadcast to sync server
            syncManager.sendSettingsChange("user_context", context, now)
        }
    }

    /**
     * Get the local file path for an attachment by its hash.
     * If the attachment is not cached locally, attempt to download it from the server.
     * Returns null if the attachment cannot be found or downloaded.
     */
    suspend fun getAttachmentPath(hash: String): String? {
        // Check if we already have it locally
        val localPath = attachmentRepository.getLocalPath(hash)
        if (localPath != null) {
            return localPath
        }

        // Not found locally - try to download from server
        Log.d("TodoListViewModel", "Attachment $hash not found locally, downloading from server...")
        return try {
            val attachmentSync = AttachmentSyncManager(attachmentRepository, certificateManager)
            attachmentSync.connect()
            val downloadedPath = attachmentSync.fetchAttachment(hash)
            attachmentSync.disconnect()

            if (downloadedPath != null) {
                Log.d("TodoListViewModel", "Downloaded attachment $hash to $downloadedPath")
                downloadedPath
            } else {
                Log.w("TodoListViewModel", "Failed to download attachment $hash from server")
                null
            }
        } catch (e: Exception) {
            Log.e("TodoListViewModel", "Error downloading attachment $hash", e)
            null
        }
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

data class ListUiState(
    val lists: List<ListDefinitionEntity> = emptyList(),
    val isLoading: Boolean = true
)
