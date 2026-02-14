// ListItemEntity.kt
//
// SPDX-License-Identifier: MIT
//
// Copyright (C) 2026 Anthony Green <green@moxielogic.com>

package com.cloodoo.app.data.local

import androidx.room.Entity
import androidx.room.Index
import androidx.room.PrimaryKey

/**
 * Room entity representing a list item row in the temporal database.
 * Mirrors the schema from the desktop cloodoo app.
 */
@Entity(
    tableName = "list_items",
    indices = [
        Index(value = ["id", "validTo"]),
        Index(value = ["listId", "validTo"]),
        Index(value = ["validFrom", "validTo"])
    ]
)
data class ListItemEntity(
    @PrimaryKey(autoGenerate = true)
    val rowId: Long = 0,
    val id: String,
    val listId: String,
    val title: String,
    val section: String? = null,
    val checked: Boolean = false,
    val notes: String? = null,
    val createdAt: String,
    val validFrom: String,
    val validTo: String? = null,
    val deviceId: String
)
