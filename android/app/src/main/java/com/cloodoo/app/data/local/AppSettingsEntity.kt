// AppSettingsEntity.kt
//
// SPDX-License-Identifier: MIT
//
// Copyright (C) 2026 Anthony Green <green@moxielogic.com>

package com.cloodoo.app.data.local

import androidx.room.ColumnInfo
import androidx.room.Entity
import androidx.room.PrimaryKey

/**
 * Entity for app-wide settings stored in the database.
 * Used for settings that need to sync across devices.
 */
@Entity(tableName = "app_settings")
data class AppSettingsEntity(
    @PrimaryKey
    @ColumnInfo(name = "key")
    val key: String,

    @ColumnInfo(name = "value")
    val value: String,

    @ColumnInfo(name = "updated_at")
    val updatedAt: String
)
