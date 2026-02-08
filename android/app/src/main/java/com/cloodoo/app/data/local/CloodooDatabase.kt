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
import androidx.room.migration.Migration
import androidx.sqlite.db.SupportSQLiteDatabase

@Database(
    entities = [TodoEntity::class, PendingSyncEntity::class, AppSettingsEntity::class, AttachmentEntity::class],
    version = 5,
    exportSchema = true
)
abstract class CloodooDatabase : RoomDatabase() {
    abstract fun todoDao(): TodoDao
    abstract fun pendingSyncDao(): PendingSyncDao
    abstract fun appSettingsDao(): AppSettingsDao
    abstract fun attachmentDao(): AttachmentDao

    companion object {
        @Volatile
        private var INSTANCE: CloodooDatabase? = null

        /**
         * Migration from version 3 to 4: Add app_settings table for cross-device settings
         */
        private val MIGRATION_3_4 = object : Migration(3, 4) {
            override fun migrate(db: SupportSQLiteDatabase) {
                db.execSQL("""
                    CREATE TABLE IF NOT EXISTS app_settings (
                        key TEXT NOT NULL PRIMARY KEY,
                        value TEXT NOT NULL,
                        updated_at TEXT NOT NULL
                    )
                """.trimIndent())
            }
        }

        /**
         * Migration from version 4 to 5: Add attachments table and attachment_hashes column
         */
        private val MIGRATION_4_5 = object : Migration(4, 5) {
            override fun migrate(db: SupportSQLiteDatabase) {
                // Add attachments table
                db.execSQL("""
                    CREATE TABLE IF NOT EXISTS attachments (
                        hash TEXT NOT NULL PRIMARY KEY,
                        content_type TEXT NOT NULL,
                        file_size INTEGER NOT NULL,
                        local_path TEXT,
                        synced_to_server INTEGER NOT NULL DEFAULT 0,
                        created_at TEXT NOT NULL
                    )
                """.trimIndent())

                // Add attachment_hashes column to todos table (nullable)
                db.execSQL("ALTER TABLE todos ADD COLUMN attachment_hashes TEXT DEFAULT NULL")
            }
        }

        fun getDatabase(context: Context): CloodooDatabase {
            return INSTANCE ?: synchronized(this) {
                val instance = Room.databaseBuilder(
                    context.applicationContext,
                    CloodooDatabase::class.java,
                    "cloodoo.db"
                )
                    .addMigrations(MIGRATION_3_4, MIGRATION_4_5)
                    .build()
                INSTANCE = instance
                instance
            }
        }
    }
}
