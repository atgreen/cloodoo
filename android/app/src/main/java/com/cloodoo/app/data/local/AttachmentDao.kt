// AttachmentDao.kt
//
// SPDX-License-Identifier: MIT
//
// Copyright (C) 2026 Anthony Green <green@moxielogic.com>

package com.cloodoo.app.data.local

import androidx.room.Dao
import androidx.room.Insert
import androidx.room.OnConflictStrategy
import androidx.room.Query
import kotlinx.coroutines.flow.Flow

@Dao
interface AttachmentDao {
    @Insert(onConflict = OnConflictStrategy.REPLACE)
    suspend fun insert(attachment: AttachmentEntity)

    @Query("SELECT * FROM attachments WHERE hash = :hash")
    suspend fun getByHash(hash: String): AttachmentEntity?

    @Query("SELECT * FROM attachments WHERE syncStatus = :status")
    suspend fun getBySyncStatus(status: String): List<AttachmentEntity>

    @Query("SELECT * FROM attachments WHERE syncStatus = 'pending_upload'")
    suspend fun getPendingUploads(): List<AttachmentEntity>

    @Query("UPDATE attachments SET syncStatus = :status WHERE hash = :hash")
    suspend fun updateSyncStatus(hash: String, status: String)

    @Query("UPDATE attachments SET localPath = :localPath, syncStatus = :status WHERE hash = :hash")
    suspend fun updateLocalPath(hash: String, localPath: String?, status: String)

    @Query("SELECT * FROM attachments")
    fun getAllAttachments(): Flow<List<AttachmentEntity>>

    @Query("DELETE FROM attachments WHERE hash = :hash")
    suspend fun delete(hash: String)
}
