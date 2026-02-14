// ListDefinitionEntity.kt
//
// SPDX-License-Identifier: MIT
//
// Copyright (C) 2026 Anthony Green <green@moxielogic.com>

package com.cloodoo.app.data.local

import androidx.room.Entity
import androidx.room.Index
import androidx.room.PrimaryKey

/**
 * Room entity representing a list definition row in the temporal database.
 * Mirrors the schema from the desktop cloodoo app.
 */
@Entity(
    tableName = "list_definitions",
    indices = [
        Index(value = ["id", "validTo"]),
        Index(value = ["nameNormalized", "validTo"], unique = true),
        Index(value = ["validFrom", "validTo"])
    ]
)
data class ListDefinitionEntity(
    @PrimaryKey(autoGenerate = true)
    val rowId: Long = 0,
    val id: String,
    val name: String,
    val nameNormalized: String,  // LOWER(name) for case-insensitive uniqueness
    val description: String? = null,
    val sections: String? = null,  // JSON array stored as string
    val createdAt: String,
    val validFrom: String,
    val validTo: String? = null,
    val deviceId: String
)
