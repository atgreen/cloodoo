package com.cloodoo.app.data.repository

import com.cloodoo.app.data.local.CloodooDatabase
import com.cloodoo.app.data.local.TodoEntity
import com.cloodoo.app.data.remote.SyncApiClient
import com.cloodoo.app.data.remote.SyncPostRequest
import com.cloodoo.app.data.remote.SyncRow
import kotlinx.coroutines.flow.Flow
import java.time.ZonedDateTime
import java.time.format.DateTimeFormatter
import java.util.UUID

/**
 * Repository that coordinates local database operations and remote sync.
 */
class TodoRepository(
    private val database: CloodooDatabase,
    private val deviceId: String
) {
    private val todoDao = database.todoDao()

    /**
     * Get current (non-superseded) TODOs as a Flow.
     */
    fun getCurrentTodos(): Flow<List<TodoEntity>> = todoDao.getCurrentTodos()

    /**
     * Create a new TODO.
     */
    suspend fun createTodo(
        title: String,
        description: String? = null,
        priority: String = "medium",
        dueDate: String? = null,
        tags: String? = null
    ): TodoEntity {
        val now = nowIso()
        val todoId = generateId()

        val entity = TodoEntity(
            id = todoId,
            title = title,
            description = description,
            priority = priority,
            dueDate = dueDate,
            tags = tags,
            createdAt = now,
            validFrom = now,
            deviceId = deviceId
        )

        todoDao.insert(entity)
        return entity
    }

    /**
     * Update an existing TODO (creates new version, marks old as superseded).
     */
    suspend fun updateTodo(
        todoId: String,
        title: String? = null,
        description: String? = null,
        priority: String? = null,
        status: String? = null,
        dueDate: String? = null,
        tags: String? = null,
        completedAt: String? = null
    ) {
        val current = todoDao.getCurrentById(todoId) ?: return
        val now = nowIso()

        // Mark current version as superseded
        todoDao.markSuperseded(todoId, now)

        // Insert new version
        val updated = current.copy(
            rowId = 0,  // Let Room auto-generate
            title = title ?: current.title,
            description = description ?: current.description,
            priority = priority ?: current.priority,
            status = status ?: current.status,
            dueDate = dueDate ?: current.dueDate,
            tags = tags ?: current.tags,
            completedAt = completedAt ?: current.completedAt,
            validFrom = now,
            validTo = null,
            deviceId = deviceId
        )

        todoDao.insert(updated)
    }

    /**
     * Mark a TODO as completed.
     */
    suspend fun completeTodo(todoId: String) {
        val now = nowIso()
        updateTodo(todoId, status = "completed", completedAt = now)
    }

    /**
     * Delete a TODO (marks all versions as superseded).
     */
    suspend fun deleteTodo(todoId: String) {
        val now = nowIso()
        todoDao.markSuperseded(todoId, now)
    }

    /**
     * Sync with remote server.
     * Returns a SyncResult with counts of received and sent rows.
     */
    suspend fun sync(serverUrl: String, lastSyncTime: String?): SyncResult {
        val client = SyncApiClient(serverUrl)

        try {
            // Step 1: Get rows from server since last sync
            val getResponse = client.getSync(lastSyncTime).getOrThrow()
            var receivedCount = 0

            // Step 2: Merge received rows into local DB
            for (row in getResponse.rows) {
                val exists = todoDao.rowExists(row.id, row.validFrom, row.deviceId)
                if (exists == 0) {
                    // If this is a "current" row (validTo is null), mark any existing
                    // current row for this todo as superseded to prevent duplicates
                    if (row.validTo == null) {
                        val existingCurrent = todoDao.getCurrentById(row.id)
                        if (existingCurrent != null) {
                            // New row is more recent - supersede the old one
                            if (row.validFrom > existingCurrent.validFrom) {
                                todoDao.markSuperseded(row.id, row.validFrom)
                            } else {
                                // Existing row is more recent - skip this incoming row
                                continue
                            }
                        }
                    }
                    todoDao.insert(row.toEntity())
                    receivedCount++
                }
            }

            // Step 3: Get local rows since last sync and send to server
            val localRows = if (lastSyncTime != null) {
                todoDao.getRowsSince(lastSyncTime)
            } else {
                todoDao.getAllRows()
            }

            var sentCount = 0
            if (localRows.isNotEmpty()) {
                val syncRows = localRows.map { it.toSyncRow() }
                val postResponse = client.postSync(
                    SyncPostRequest(deviceId = deviceId, rows = syncRows)
                ).getOrThrow()
                sentCount = postResponse.accepted
            }

            return SyncResult(
                success = true,
                receivedRows = receivedCount,
                sentRows = sentCount,
                serverTime = getResponse.serverTime
            )
        } catch (e: Exception) {
            return SyncResult(
                success = false,
                error = e.message ?: "Unknown error"
            )
        } finally {
            client.close()
        }
    }

    private fun nowIso(): String {
        return ZonedDateTime.now().format(DateTimeFormatter.ISO_OFFSET_DATE_TIME)
    }

    private fun generateId(): String {
        return "${System.currentTimeMillis()}-${(0..99999).random()}"
    }

    private fun SyncRow.toEntity(): TodoEntity {
        return TodoEntity(
            id = id,
            title = title,
            description = description,
            priority = priority,
            status = status,
            scheduledDate = scheduledDate,
            dueDate = dueDate,
            tags = tags,
            estimatedMinutes = estimatedMinutes,
            locationInfo = locationInfo,
            url = url,
            parentId = parentId,
            createdAt = createdAt,
            completedAt = completedAt,
            validFrom = validFrom,
            validTo = validTo,
            deviceId = deviceId
        )
    }

    private fun TodoEntity.toSyncRow(): SyncRow {
        return SyncRow(
            rowId = rowId,
            id = id,
            title = title,
            description = description,
            priority = priority,
            status = status,
            scheduledDate = scheduledDate,
            dueDate = dueDate,
            tags = tags,
            estimatedMinutes = estimatedMinutes,
            locationInfo = locationInfo,
            url = url,
            parentId = parentId,
            createdAt = createdAt,
            completedAt = completedAt,
            validFrom = validFrom,
            validTo = validTo,
            deviceId = deviceId
        )
    }
}

data class SyncResult(
    val success: Boolean,
    val receivedRows: Int = 0,
    val sentRows: Int = 0,
    val serverTime: String? = null,
    val error: String? = null
)
