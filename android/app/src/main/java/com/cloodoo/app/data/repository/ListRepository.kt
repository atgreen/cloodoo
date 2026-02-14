// ListRepository.kt
//
// SPDX-License-Identifier: MIT
//
// Copyright (C) 2026 Anthony Green <green@moxielogic.com>

package com.cloodoo.app.data.repository

import com.cloodoo.app.data.local.CloodooDatabase
import com.cloodoo.app.data.local.ListDefinitionEntity
import com.cloodoo.app.data.local.ListItemEntity
import kotlinx.coroutines.flow.Flow
import java.time.ZonedDateTime
import java.time.format.DateTimeFormatter

/**
 * Repository that coordinates local database operations for lists.
 * Sync is handled separately by SyncManager via gRPC streaming.
 */
class ListRepository(
    private val database: CloodooDatabase,
    private val deviceId: String
) {
    private val listDao = database.listDao()

    fun getListDefinitions(): Flow<List<ListDefinitionEntity>> =
        listDao.getCurrentListDefinitions()

    fun getListItems(listId: String): Flow<List<ListItemEntity>> =
        listDao.getListItemsForList(listId)

    suspend fun getListDefinitionById(id: String): ListDefinitionEntity? =
        listDao.getCurrentListDefinitionById(id)

    suspend fun getListItemById(id: String): ListItemEntity? =
        listDao.getCurrentListItemById(id)

    suspend fun createListDefinition(
        name: String,
        description: String? = null,
        sections: String? = null
    ): ListDefinitionEntity {
        val now = nowIso()
        val entity = ListDefinitionEntity(
            id = generateId(),
            name = name,
            nameNormalized = name.lowercase(),
            description = description,
            sections = sections,
            createdAt = now,
            validFrom = now,
            deviceId = deviceId
        )
        listDao.insertDefinition(entity)
        return entity
    }

    suspend fun updateListDefinition(
        id: String,
        name: String? = null,
        description: String? = null,
        sections: String? = null
    ): ListDefinitionEntity? {
        val current = listDao.getCurrentListDefinitionById(id) ?: return null
        val now = nowIso()

        listDao.markDefinitionSuperseded(id, now)

        val updated = current.copy(
            rowId = 0,
            name = name ?: current.name,
            nameNormalized = (name ?: current.name).lowercase(),
            description = description ?: current.description,
            sections = sections ?: current.sections,
            validFrom = now,
            validTo = null,
            deviceId = deviceId
        )
        listDao.insertDefinition(updated)
        return updated
    }

    suspend fun deleteListDefinition(id: String) {
        val now = nowIso()
        listDao.markDefinitionSuperseded(id, now)
        listDao.markAllItemsSupersededForList(id, now)
    }

    suspend fun addListItem(
        listId: String,
        title: String,
        section: String? = null,
        notes: String? = null
    ): ListItemEntity {
        val now = nowIso()
        val entity = ListItemEntity(
            id = generateId(),
            listId = listId,
            title = title,
            section = section,
            notes = notes,
            createdAt = now,
            validFrom = now,
            deviceId = deviceId
        )
        listDao.insertItem(entity)
        return entity
    }

    suspend fun toggleListItem(itemId: String): ListItemEntity? {
        val current = listDao.getCurrentListItemById(itemId) ?: return null
        val now = nowIso()

        listDao.markItemSuperseded(itemId, now)

        val updated = current.copy(
            rowId = 0,
            checked = !current.checked,
            validFrom = now,
            validTo = null,
            deviceId = deviceId
        )
        listDao.insertItem(updated)
        return updated
    }

    suspend fun updateListItem(
        itemId: String,
        title: String? = null,
        section: String? = null,
        notes: String? = null
    ): ListItemEntity? {
        val current = listDao.getCurrentListItemById(itemId) ?: return null
        val now = nowIso()

        listDao.markItemSuperseded(itemId, now)

        val updated = current.copy(
            rowId = 0,
            title = title ?: current.title,
            section = section ?: current.section,
            notes = notes ?: current.notes,
            validFrom = now,
            validTo = null,
            deviceId = deviceId
        )
        listDao.insertItem(updated)
        return updated
    }

    suspend fun getCheckedListItems(listId: String): List<ListItemEntity> =
        listDao.getCheckedItemsForList(listId)

    suspend fun deleteListItem(itemId: String) {
        val now = nowIso()
        listDao.markItemSuperseded(itemId, now)
    }

    private fun nowIso(): String {
        return ZonedDateTime.now().format(DateTimeFormatter.ISO_OFFSET_DATE_TIME)
    }

    private fun generateId(): String {
        return "${System.currentTimeMillis()}-${(0..99999).random()}"
    }
}
