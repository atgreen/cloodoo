package com.cloodoo.app.data.local

import androidx.room.Entity
import androidx.room.PrimaryKey

/**
 * Room entity for queuing sync changes while offline.
 * Entries are drained and sent to the server upon reconnect.
 */
@Entity(tableName = "pending_sync")
data class PendingSyncEntity(
    @PrimaryKey(autoGenerate = true)
    val rowId: Long = 0,
    val todoId: String,
    val changeType: String,  // "upsert" or "delete"
    val createdAt: String
)
