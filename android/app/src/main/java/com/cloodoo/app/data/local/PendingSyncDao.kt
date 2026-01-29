package com.cloodoo.app.data.local

import androidx.room.Dao
import androidx.room.Insert
import androidx.room.Query

@Dao
interface PendingSyncDao {

    @Query("SELECT * FROM pending_sync ORDER BY createdAt ASC")
    suspend fun getAll(): List<PendingSyncEntity>

    @Insert
    suspend fun insert(entry: PendingSyncEntity)

    @Query("DELETE FROM pending_sync WHERE rowId = :rowId")
    suspend fun delete(rowId: Long)

    @Query("DELETE FROM pending_sync WHERE todoId = :todoId AND changeType = :changeType")
    suspend fun deleteByTodoId(todoId: String, changeType: String)
}
