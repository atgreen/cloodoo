// AppSettingsDao.kt
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
interface AppSettingsDao {
    @Query("SELECT * FROM app_settings WHERE key = :key LIMIT 1")
    suspend fun getSetting(key: String): AppSettingsEntity?

    @Query("SELECT * FROM app_settings WHERE key = :key LIMIT 1")
    fun getSettingFlow(key: String): Flow<AppSettingsEntity?>

    @Query("SELECT * FROM app_settings")
    suspend fun getAllSettings(): List<AppSettingsEntity>

    @Insert(onConflict = OnConflictStrategy.REPLACE)
    suspend fun insertSetting(setting: AppSettingsEntity)

    @Query("DELETE FROM app_settings WHERE key = :key")
    suspend fun deleteSetting(key: String)
}
