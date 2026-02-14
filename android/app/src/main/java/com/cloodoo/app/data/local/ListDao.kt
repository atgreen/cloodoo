// ListDao.kt
//
// SPDX-License-Identifier: MIT
//
// Copyright (C) 2026 Anthony Green <green@moxielogic.com>

package com.cloodoo.app.data.local

import androidx.room.Dao
import androidx.room.Insert
import androidx.room.Query
import kotlinx.coroutines.flow.Flow

@Dao
interface ListDao {

    // ── List Definitions ──

    /**
     * Get all current (non-superseded) list definitions.
     */
    @Query("SELECT * FROM list_definitions WHERE validTo IS NULL ORDER BY name ASC")
    fun getCurrentListDefinitions(): Flow<List<ListDefinitionEntity>>

    /**
     * Get the current version of a list definition by id.
     */
    @Query("SELECT * FROM list_definitions WHERE id = :id AND validTo IS NULL LIMIT 1")
    suspend fun getCurrentListDefinitionById(id: String): ListDefinitionEntity?

    /**
     * Insert a new list definition row.
     */
    @Insert
    suspend fun insertDefinition(definition: ListDefinitionEntity): Long

    /**
     * Mark a current list definition version as superseded.
     */
    @Query("UPDATE list_definitions SET validTo = :validTo WHERE id = :id AND validTo IS NULL")
    suspend fun markDefinitionSuperseded(id: String, validTo: String)

    /**
     * Find current list definitions with the same normalized name but a different id.
     * Used to detect duplicates that need migration.
     */
    @Query("SELECT * FROM list_definitions WHERE nameNormalized = :nameNormalized AND id != :excludeId AND validTo IS NULL")
    suspend fun findDuplicateDefinitions(nameNormalized: String, excludeId: String): List<ListDefinitionEntity>

    /**
     * Supersede any current list definitions with the same normalized name but a different id.
     * Used to deduplicate when a list is replaced by a new id on the server.
     */
    @Query("UPDATE list_definitions SET validTo = :validTo WHERE nameNormalized = :nameNormalized AND id != :excludeId AND validTo IS NULL")
    suspend fun supersedeDuplicateDefinitions(nameNormalized: String, excludeId: String, validTo: String)

    /**
     * Migrate current items from one list id to another by superseding and re-inserting.
     * Returns all current items for a given list id (for migration).
     */
    @Query("SELECT * FROM list_items WHERE listId = :listId AND validTo IS NULL")
    suspend fun getCurrentItemsForList(listId: String): List<ListItemEntity>

    /**
     * Get all definition rows since a given timestamp (for sync).
     */
    @Query("SELECT * FROM list_definitions WHERE validFrom > :since ORDER BY validFrom ASC")
    suspend fun getDefinitionRowsSince(since: String): List<ListDefinitionEntity>

    // ── List Items ──

    /**
     * Get all current (non-superseded) items for a list.
     */
    @Query("SELECT * FROM list_items WHERE listId = :listId AND validTo IS NULL ORDER BY createdAt ASC")
    fun getListItemsForList(listId: String): Flow<List<ListItemEntity>>

    /**
     * Get the current version of a list item by id.
     */
    @Query("SELECT * FROM list_items WHERE id = :id AND validTo IS NULL LIMIT 1")
    suspend fun getCurrentListItemById(id: String): ListItemEntity?

    /**
     * Insert a new list item row.
     */
    @Insert
    suspend fun insertItem(item: ListItemEntity): Long

    /**
     * Mark a current list item version as superseded.
     */
    @Query("UPDATE list_items SET validTo = :validTo WHERE id = :id AND validTo IS NULL")
    suspend fun markItemSuperseded(id: String, validTo: String)

    /**
     * Mark all current items for a list as superseded (used when deleting a list).
     */
    @Query("UPDATE list_items SET validTo = :validTo WHERE listId = :listId AND validTo IS NULL")
    suspend fun markAllItemsSupersededForList(listId: String, validTo: String)

    /**
     * Get all current checked items for a list.
     */
    @Query("SELECT * FROM list_items WHERE listId = :listId AND checked = 1 AND validTo IS NULL")
    suspend fun getCheckedItemsForList(listId: String): List<ListItemEntity>

    /**
     * Get all item rows since a given timestamp (for sync).
     */
    @Query("SELECT * FROM list_items WHERE validFrom > :since ORDER BY validFrom ASC")
    suspend fun getItemRowsSince(since: String): List<ListItemEntity>
}
