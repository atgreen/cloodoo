// AttachmentEntity.kt
//
// SPDX-License-Identifier: MIT
//
// Copyright (C) 2026 Anthony Green <green@moxielogic.com>

package com.cloodoo.app.data.local

import androidx.room.Entity
import androidx.room.PrimaryKey

/**
 * Room entity representing an attachment in the local database.
 * Attachments are stored locally and synced to the server on-demand.
 */
@Entity(tableName = "attachments")
data class AttachmentEntity(
    @PrimaryKey
    val hash: String,           // SHA256 hash (content-addressed)
    val filename: String,       // Original filename
    val mimeType: String,       // MIME type (e.g., "image/jpeg")
    val size: Long,             // Size in bytes
    val createdAt: String,      // ISO 8601 timestamp
    val localPath: String?,     // Path to local file (null if not cached)
    val syncStatus: String      // "pending_upload", "uploaded", "cached"
)
