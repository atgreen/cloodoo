// CloodooDatabase.kt
//
// SPDX-License-Identifier: MIT
//
// Copyright (C) 2026 Anthony Green <green@moxielogic.com>

package com.cloodoo.app.data.local

import android.content.Context
import androidx.room.Database
import androidx.room.Room
import androidx.room.RoomDatabase

@Database(
    entities = [TodoEntity::class, PendingSyncEntity::class, AppSettingsEntity::class, AttachmentEntity::class, ListDefinitionEntity::class, ListItemEntity::class],
    version = 8,
    exportSchema = false
)
abstract class CloodooDatabase : RoomDatabase() {
    abstract fun todoDao(): TodoDao
    abstract fun pendingSyncDao(): PendingSyncDao
    abstract fun appSettingsDao(): AppSettingsDao
    abstract fun attachmentDao(): AttachmentDao
    abstract fun listDao(): ListDao

    companion object {
        @Volatile
        private var INSTANCE: CloodooDatabase? = null

        fun getDatabase(context: Context): CloodooDatabase {
            return INSTANCE ?: synchronized(this) {
                val instance = Room.databaseBuilder(
                    context.applicationContext,
                    CloodooDatabase::class.java,
                    "cloodoo.db"
                )
                    .fallbackToDestructiveMigration()
                    .build()
                INSTANCE = instance
                instance
            }
        }
    }
}
