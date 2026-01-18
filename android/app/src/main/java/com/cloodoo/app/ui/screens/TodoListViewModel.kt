package com.cloodoo.app.ui.screens

import android.app.Application
import androidx.lifecycle.AndroidViewModel
import androidx.lifecycle.viewModelScope
import com.cloodoo.app.data.local.CloodooDatabase
import com.cloodoo.app.data.local.TodoEntity
import com.cloodoo.app.data.repository.SyncResult
import com.cloodoo.app.data.repository.TodoRepository
import kotlinx.coroutines.flow.*
import kotlinx.coroutines.launch
import java.util.UUID

class TodoListViewModel(application: Application) : AndroidViewModel(application) {

    private val deviceId = loadOrCreateDeviceId(application)
    private val database = CloodooDatabase.getDatabase(application)
    private val repository = TodoRepository(database, deviceId)

    // UI State
    private val _uiState = MutableStateFlow(TodoListUiState())
    val uiState: StateFlow<TodoListUiState> = _uiState.asStateFlow()

    // Settings
    private val _serverUrl = MutableStateFlow("")
    val serverUrl: StateFlow<String> = _serverUrl.asStateFlow()

    private val _lastSyncTime = MutableStateFlow<String?>(null)

    init {
        // Load server URL from preferences
        loadSettings(application)

        // Observe current TODOs
        viewModelScope.launch {
            repository.getCurrentTodos().collect { todos ->
                _uiState.update { it.copy(todos = todos, isLoading = false) }
            }
        }
    }

    fun createTodo(title: String, priority: String = "medium", dueDate: String? = null) {
        if (title.isBlank()) return

        viewModelScope.launch {
            repository.createTodo(
                title = title.trim(),
                priority = priority,
                dueDate = dueDate
            )
        }
    }

    fun toggleComplete(todoId: String) {
        viewModelScope.launch {
            val todo = _uiState.value.todos.find { it.id == todoId }
            if (todo != null) {
                if (todo.status == "completed") {
                    repository.updateTodo(todoId, status = "pending")
                } else {
                    repository.completeTodo(todoId)
                }
            }
        }
    }

    fun deleteTodo(todoId: String) {
        viewModelScope.launch {
            repository.deleteTodo(todoId)
        }
    }

    fun setServerUrl(url: String) {
        _serverUrl.value = url
        saveSettings(getApplication())
    }

    fun sync() {
        val url = _serverUrl.value
        if (url.isBlank()) {
            _uiState.update { it.copy(syncError = "Server URL not configured") }
            return
        }

        viewModelScope.launch {
            _uiState.update { it.copy(isSyncing = true, syncError = null) }

            val result = repository.sync(url, _lastSyncTime.value)

            if (result.success) {
                result.serverTime?.let { _lastSyncTime.value = it }
                saveSettings(getApplication())
                _uiState.update {
                    it.copy(
                        isSyncing = false,
                        lastSyncResult = "Synced: ${result.receivedRows} received, ${result.sentRows} sent"
                    )
                }
            } else {
                _uiState.update {
                    it.copy(
                        isSyncing = false,
                        syncError = result.error ?: "Sync failed"
                    )
                }
            }
        }
    }

    fun clearSyncMessage() {
        _uiState.update { it.copy(lastSyncResult = null, syncError = null) }
    }

    private fun loadOrCreateDeviceId(application: Application): String {
        val prefs = application.getSharedPreferences("cloodoo_prefs", 0)
        var id = prefs.getString("device_id", null)
        if (id == null) {
            id = UUID.randomUUID().toString()
            prefs.edit().putString("device_id", id).apply()
        }
        return id
    }

    private fun loadSettings(application: Application) {
        val prefs = application.getSharedPreferences("cloodoo_prefs", 0)
        _serverUrl.value = prefs.getString("server_url", "") ?: ""
        _lastSyncTime.value = prefs.getString("last_sync_time", null)
    }

    private fun saveSettings(application: Application) {
        val prefs = application.getSharedPreferences("cloodoo_prefs", 0)
        prefs.edit()
            .putString("server_url", _serverUrl.value)
            .putString("last_sync_time", _lastSyncTime.value)
            .apply()
    }
}

data class TodoListUiState(
    val todos: List<TodoEntity> = emptyList(),
    val isLoading: Boolean = true,
    val isSyncing: Boolean = false,
    val lastSyncResult: String? = null,
    val syncError: String? = null
)
