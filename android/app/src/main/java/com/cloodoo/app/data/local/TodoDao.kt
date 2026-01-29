// TodoDao.kt
//
// SPDX-License-Identifier: MIT
//
// Copyright (C) 2026 Anthony Green <green@moxielogic.com>

package com.cloodoo.app.data.local

import androidx.room.Dao
import androidx.room.Insert
import androidx.room.Query
import kotlinx.coroutines.flow.Flow

@Dao
interface TodoDao {

    /**
     * Get all current (non-superseded) TODOs.
     */
    @Query("SELECT * FROM todos WHERE validTo IS NULL ORDER BY createdAt DESC")
    fun getCurrentTodos(): Flow<List<TodoEntity>>

    /**
     * Get all rows since a given timestamp (for sync).
     */
    @Query("SELECT * FROM todos WHERE validFrom > :since ORDER BY validFrom ASC")
    suspend fun getRowsSince(since: String): List<TodoEntity>

    /**
     * Check if a row already exists (by id, validFrom, and deviceId).
     */
    @Query("SELECT COUNT(*) FROM todos WHERE id = :id AND validFrom = :validFrom AND deviceId = :deviceId")
    suspend fun rowExists(id: String, validFrom: String, deviceId: String): Int

    /**
     * Insert a new row.
     */
    @Insert
    suspend fun insert(todo: TodoEntity): Long

    /**
     * Insert multiple rows.
     */
    @Insert
    suspend fun insertAll(todos: List<TodoEntity>)

    /**
     * Mark a current version as superseded.
     */
    @Query("UPDATE todos SET validTo = :validTo WHERE id = :id AND validTo IS NULL")
    suspend fun markSuperseded(id: String, validTo: String)

    /**
     * Get the current version of a TODO by id.
     */
    @Query("SELECT * FROM todos WHERE id = :id AND validTo IS NULL LIMIT 1")
    suspend fun getCurrentById(id: String): TodoEntity?

    /**
     * Count current (non-superseded) TODOs.
     */
    @Query("SELECT COUNT(*) FROM todos WHERE validTo IS NULL")
    suspend fun countCurrent(): Int

    /**
     * Get all TODOs (for debugging).
     */
    @Query("SELECT * FROM todos ORDER BY validFrom DESC")
    suspend fun getAllRows(): List<TodoEntity>

    /**
     * Delete all rows (for testing/reset).
     */
    @Query("DELETE FROM todos")
    suspend fun deleteAll()
}
