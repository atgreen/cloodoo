// TodoRepository.kt
//
// SPDX-License-Identifier: MIT
//
// Copyright (C) 2026 Anthony Green <green@moxielogic.com>

package com.cloodoo.app.data.repository

import com.cloodoo.app.data.local.CloodooDatabase
import com.cloodoo.app.data.local.TodoEntity
import kotlinx.coroutines.flow.Flow
import java.time.ZonedDateTime
import java.time.format.DateTimeFormatter

/**
 * Repository that coordinates local database operations.
 * Sync is handled separately by SyncManager via gRPC streaming.
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
     * Check if there are no current TODOs in the local database.
     */
    suspend fun isEmpty(): Boolean = todoDao.countCurrent() == 0

    /**
     * Create a new TODO.
     */
    suspend fun createTodo(
        title: String,
        description: String? = null,
        priority: String = "medium",
        dueDate: String? = null,
        scheduledDate: String? = null,
        tags: String? = null,
        repeatInterval: Int? = null,
        repeatUnit: String? = null
    ): TodoEntity {
        val now = nowIso()
        val todoId = generateId()

        val entity = TodoEntity(
            id = todoId,
            title = title,
            description = description,
            priority = priority,
            dueDate = dueDate,
            scheduledDate = scheduledDate,
            tags = tags,
            repeatInterval = repeatInterval,
            repeatUnit = repeatUnit,
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
        scheduledDate: String? = null,
        tags: String? = null,
        completedAt: String? = null,
        repeatInterval: Int? = null,
        repeatUnit: String? = null
    ) {
        val current = todoDao.getCurrentById(todoId) ?: return
        val now = nowIso()

        // Mark current version as superseded
        todoDao.markSuperseded(todoId, now)

        // Insert new version
        // Convention: null = keep current, "" = clear (set to null), other = set value
        val updated = current.copy(
            rowId = 0,  // Let Room auto-generate
            title = title ?: current.title,
            description = description ?: current.description,
            priority = priority ?: current.priority,
            status = status ?: current.status,
            dueDate = when (dueDate) { null -> current.dueDate; "" -> null; else -> dueDate },
            scheduledDate = when (scheduledDate) { null -> current.scheduledDate; "" -> null; else -> scheduledDate },
            tags = when (tags) { null -> current.tags; "" -> null; else -> tags },
            completedAt = completedAt ?: current.completedAt,
            repeatInterval = repeatInterval ?: current.repeatInterval,
            repeatUnit = repeatUnit ?: current.repeatUnit,
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

    private fun nowIso(): String {
        return ZonedDateTime.now().format(DateTimeFormatter.ISO_OFFSET_DATE_TIME)
    }

    private fun generateId(): String {
        return "${System.currentTimeMillis()}-${(0..99999).random()}"
    }
}
