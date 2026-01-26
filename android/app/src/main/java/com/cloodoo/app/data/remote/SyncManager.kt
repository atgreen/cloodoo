package com.cloodoo.app.data.remote

import android.util.Log
import com.cloodoo.app.data.local.CloodooDatabase
import com.cloodoo.app.data.local.TodoEntity
import com.cloodoo.app.data.security.CertificateManager
import com.cloodoo.app.proto.CloodooSync.*
import kotlinx.coroutines.*
import kotlinx.coroutines.flow.*

/**
 * Manages bidirectional gRPC sync with the server.
 * Handles connection lifecycle, message routing, and local database updates.
 */
class SyncManager(
    private val database: CloodooDatabase,
    private val certificateManager: CertificateManager,
    private val deviceId: String
) {
    companion object {
        private const val TAG = "SyncManager"
        private const val INITIAL_RECONNECT_DELAY_MS = 1000L
        private const val MAX_RECONNECT_DELAY_MS = 30000L
        private const val RECONNECT_BACKOFF_MULTIPLIER = 2.0
    }

    private val todoDao = database.todoDao()
    private var grpcClient: GrpcSyncClient? = null
    private var syncJob: Job? = null
    private var reconnectJob: Job? = null
    private var autoReconnect = true
    private var currentReconnectDelay = INITIAL_RECONNECT_DELAY_MS

    private val _connectionState = MutableStateFlow(ConnectionState.DISCONNECTED)
    val connectionState: StateFlow<ConnectionState> = _connectionState.asStateFlow()

    private val _syncEvents = MutableSharedFlow<SyncEvent>()
    val syncEvents: SharedFlow<SyncEvent> = _syncEvents.asSharedFlow()

    private var connectionScope: CoroutineScope? = null

    /**
     * Start the sync connection.
     * @param since Optional timestamp to sync from (ISO 8601), null for full sync
     */
    fun connect(since: String? = null, scope: CoroutineScope) {
        if (syncJob?.isActive == true) {
            Log.d(TAG, "Already connected or connecting")
            return
        }

        autoReconnect = true
        connectionScope = scope
        reconnectJob?.cancel()
        startConnection(since, scope)
    }

    private fun startConnection(since: String? = null, scope: CoroutineScope) {
        syncJob = scope.launch {
            try {
                _connectionState.value = ConnectionState.CONNECTING
                Log.d(TAG, "Starting sync connection...")

                grpcClient = GrpcSyncClient(certificateManager)

                grpcClient!!.connect(deviceId, since).collect { message ->
                    // Reset reconnect delay on successful message
                    currentReconnectDelay = INITIAL_RECONNECT_DELAY_MS
                    handleServerMessage(message)
                }

                _connectionState.value = ConnectionState.DISCONNECTED
                Log.d(TAG, "Sync connection closed normally")
                scheduleReconnect(scope)
            } catch (e: CancellationException) {
                Log.d(TAG, "Sync connection cancelled")
                _connectionState.value = ConnectionState.DISCONNECTED
            } catch (e: Exception) {
                Log.e(TAG, "Sync connection error", e)
                _connectionState.value = ConnectionState.ERROR
                _syncEvents.emit(SyncEvent.Error(e.message ?: "Connection error"))
                scheduleReconnect(scope)
            }
        }
    }

    private fun scheduleReconnect(scope: CoroutineScope) {
        if (!autoReconnect) {
            Log.d(TAG, "Auto-reconnect disabled, not reconnecting")
            return
        }

        reconnectJob?.cancel()
        reconnectJob = scope.launch {
            Log.d(TAG, "Scheduling reconnect in ${currentReconnectDelay}ms")
            delay(currentReconnectDelay)

            // Exponential backoff
            currentReconnectDelay = (currentReconnectDelay * RECONNECT_BACKOFF_MULTIPLIER)
                .toLong()
                .coerceAtMost(MAX_RECONNECT_DELAY_MS)

            Log.d(TAG, "Attempting reconnect...")
            startConnection(null, scope)
        }
    }

    /**
     * Disconnect from the server.
     */
    fun disconnect() {
        autoReconnect = false
        reconnectJob?.cancel()
        reconnectJob = null
        syncJob?.cancel()
        grpcClient?.disconnect()
        grpcClient = null
        _connectionState.value = ConnectionState.DISCONNECTED
        currentReconnectDelay = INITIAL_RECONNECT_DELAY_MS
    }

    /**
     * Send a local todo change to the server.
     */
    suspend fun sendTodoUpsert(todo: TodoEntity) {
        val client = grpcClient ?: run {
            Log.w(TAG, "Not connected, cannot send upsert")
            return
        }

        val todoData = todo.toProto()
        client.sendUpsert(deviceId, todoData)
        _syncEvents.emit(SyncEvent.Sent(todo.id))
    }

    /**
     * Send a delete request to the server.
     */
    suspend fun sendTodoDelete(todoId: String) {
        val client = grpcClient ?: run {
            Log.w(TAG, "Not connected, cannot send delete")
            return
        }

        client.sendDelete(deviceId, todoId)
        _syncEvents.emit(SyncEvent.Sent(todoId))
    }

    private suspend fun handleServerMessage(message: SyncMessage) {
        when (message.msgCase) {
            SyncMessage.MsgCase.ACK -> {
                val ack = message.ack
                Log.d(TAG, "Received ACK: serverTime=${ack.serverTime}, pending=${ack.pendingChanges}, error=${ack.error}")

                // Check if server rejected the connection (e.g., clock skew)
                if (ack.error.isNotEmpty()) {
                    Log.e(TAG, "Server rejected connection: ${ack.error}")
                    _connectionState.value = ConnectionState.ERROR
                    _syncEvents.emit(SyncEvent.Error(ack.error))
                    // Don't auto-reconnect for clock skew errors - user needs to fix their clock
                    autoReconnect = false
                    grpcClient?.disconnect()
                    return
                }

                _connectionState.value = ConnectionState.CONNECTED
                _syncEvents.emit(SyncEvent.Connected(ack.serverTime, ack.pendingChanges))
            }

            SyncMessage.MsgCase.CHANGE -> {
                val change = message.change
                Log.d(TAG, "Received change from ${change.deviceId}: ${change.changeCase}")

                when (change.changeCase) {
                    TodoChange.ChangeCase.UPSERT -> {
                        val todoData = change.upsert
                        handleRemoteUpsert(todoData, change.timestamp)
                    }
                    TodoChange.ChangeCase.DELETE_ID -> {
                        val todoId = change.deleteId
                        handleRemoteDelete(todoId, change.timestamp)
                    }
                    else -> {
                        Log.w(TAG, "Unknown change type: ${change.changeCase}")
                    }
                }
            }

            else -> {
                Log.w(TAG, "Unknown message type: ${message.msgCase}")
            }
        }
    }

    private suspend fun handleRemoteUpsert(todoData: TodoData, timestamp: String) {
        val entity = todoData.toEntity(timestamp)

        // Check if we already have a more recent version
        val existing = todoDao.getCurrentById(entity.id)
        if (existing != null && existing.validFrom >= entity.validFrom) {
            Log.d(TAG, "Skipping remote upsert - local version is newer")
            return
        }

        // Mark existing as superseded and insert new version
        if (existing != null) {
            todoDao.markSuperseded(entity.id, entity.validFrom)
        }
        todoDao.insert(entity)

        Log.d(TAG, "Applied remote upsert for ${entity.id}")
        _syncEvents.emit(SyncEvent.Received(entity.id, SyncEventType.UPSERT))
    }

    private suspend fun handleRemoteDelete(todoId: String, timestamp: String) {
        val existing = todoDao.getCurrentById(todoId)
        if (existing != null) {
            todoDao.markSuperseded(todoId, timestamp)
            Log.d(TAG, "Applied remote delete for $todoId")
            _syncEvents.emit(SyncEvent.Received(todoId, SyncEventType.DELETE))
        }
    }

    private fun TodoEntity.toProto(): TodoData {
        return TodoData.newBuilder()
            .setId(id)
            .setTitle(title)
            .setDescription(description ?: "")
            .setPriority(priority)
            .setStatus(status)
            .setScheduledDate(scheduledDate ?: "")
            .setDueDate(dueDate ?: "")
            .addAllTags(tags?.split(",")?.filter { it.isNotBlank() } ?: emptyList())
            .setEstimatedMinutes(estimatedMinutes ?: 0)
            .setUrl(url ?: "")
            .setCreatedAt(createdAt)
            .setCompletedAt(completedAt ?: "")
            .setParentId(parentId ?: "")
            .build()
    }

    private fun TodoData.toEntity(validFrom: String): TodoEntity {
        return TodoEntity(
            id = id,
            title = title,
            description = description.ifEmpty { null },
            priority = priority,
            status = status,
            scheduledDate = scheduledDate.ifEmpty { null },
            dueDate = dueDate.ifEmpty { null },
            tags = tagsList.joinToString(",").ifEmpty { null },
            estimatedMinutes = if (estimatedMinutes > 0) estimatedMinutes else null,
            url = url.ifEmpty { null },
            parentId = parentId.ifEmpty { null },
            createdAt = createdAt,
            completedAt = completedAt.ifEmpty { null },
            validFrom = validFrom,
            validTo = null,
            deviceId = deviceId
        )
    }
}

enum class ConnectionState {
    DISCONNECTED,
    CONNECTING,
    CONNECTED,
    ERROR
}

sealed class SyncEvent {
    data class Connected(val serverTime: String, val pendingChanges: Int) : SyncEvent()
    data class Received(val todoId: String, val type: SyncEventType) : SyncEvent()
    data class Sent(val todoId: String) : SyncEvent()
    data class Error(val message: String) : SyncEvent()
}

enum class SyncEventType {
    UPSERT,
    DELETE
}
