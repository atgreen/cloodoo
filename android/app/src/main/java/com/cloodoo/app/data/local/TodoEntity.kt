// TodoEntity.kt
//
// SPDX-License-Identifier: MIT
//
// Copyright (C) 2026 Anthony Green <green@moxielogic.com>

package com.cloodoo.app.data.local

import androidx.room.Entity
import androidx.room.Index
import androidx.room.PrimaryKey

/**
 * Room entity representing a TODO row in the temporal database.
 * Mirrors the schema from the desktop cloodoo app.
 */
@Entity(
    tableName = "todos",
    indices = [
        Index(value = ["id", "validTo"]),
        Index(value = ["validFrom", "validTo"]),
        Index(value = ["validFrom"])
    ]
)
data class TodoEntity(
    @PrimaryKey(autoGenerate = true)
    val rowId: Long = 0,
    val id: String,
    val title: String,
    val description: String? = null,
    val priority: String = "medium",
    val status: String = "pending",
    val scheduledDate: String? = null,
    val dueDate: String? = null,
    val tags: String? = null,  // JSON array stored as string
    val locationInfo: String? = null,  // JSON object stored as string
    val url: String? = null,
    val parentId: String? = null,
    val repeatInterval: Int? = null,
    val repeatUnit: String? = null,  // "day", "week", "month", "year"
    val attachmentHashes: String? = null,  // JSON array stored as string
    val enrichingP: Boolean = false,  // True if server-side LLM enrichment requested
    val createdAt: String,
    val completedAt: String? = null,
    val validFrom: String,
    val validTo: String? = null,
    val deviceId: String
)
