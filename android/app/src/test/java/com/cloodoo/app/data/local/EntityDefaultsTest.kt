// EntityDefaultsTest.kt
//
// SPDX-License-Identifier: MIT
//
// Copyright (C) 2026 Anthony Green <green@moxielogic.com>

package com.cloodoo.app.data.local

import org.junit.Assert.assertEquals
import org.junit.Assert.assertFalse
import org.junit.Assert.assertNull
import org.junit.Test

/**
 * The sync protocol and offline queue depend on these entity defaults:
 * new rows must be current (validTo == null), auto-generated keys must
 * start at 0 so Room assigns them, and queue entries must carry their
 * change type verbatim. These tests pin those contracts.
 */
class EntityDefaultsTest {

    @Test
    fun pendingSyncEntityDefaultsRowIdToZeroForAutoGenerate() {
        val entry = PendingSyncEntity(
            todoId = "todo-1",
            changeType = "upsert",
            createdAt = "2026-01-26T14:07:39Z"
        )
        assertEquals(0L, entry.rowId)
        assertEquals("todo-1", entry.todoId)
        assertEquals("upsert", entry.changeType)
    }

    @Test
    fun pendingSyncEntityPreservesChangeTypeVerbatim() {
        val upsert = PendingSyncEntity(todoId = "t", changeType = "upsert", createdAt = "x")
        val delete = upsert.copy(changeType = "delete")
        assertEquals("upsert", upsert.changeType)
        assertEquals("delete", delete.changeType)
    }

    @Test
    fun todoEntityNewRowIsCurrentByDefault() {
        val todo = TodoEntity(
            id = "todo-1",
            title = "Test",
            createdAt = "2026-01-26T14:07:39Z",
            validFrom = "2026-01-26T14:07:39Z",
            deviceId = "device-1"
        )
        assertNull(todo.validTo)
        assertEquals(0L, todo.rowId)
    }

    @Test
    fun todoEntityDefaultsMatchDesktopSchema() {
        val todo = TodoEntity(
            id = "todo-1",
            title = "Test",
            createdAt = "2026-01-26T14:07:39Z",
            validFrom = "2026-01-26T14:07:39Z",
            deviceId = "device-1"
        )
        assertEquals("medium", todo.priority)
        assertEquals("pending", todo.status)
        assertFalse(todo.enrichingP)
        assertNull(todo.completedAt)
        assertNull(todo.repeatInterval)
    }

    @Test
    fun todoEntitySupersededCopyKeepsIdentity() {
        val current = TodoEntity(
            id = "todo-1",
            title = "Test",
            createdAt = "2026-01-26T14:07:39Z",
            validFrom = "2026-01-26T14:07:39Z",
            deviceId = "device-1"
        )
        val superseded = current.copy(validTo = "2026-01-27T00:00:00Z")
        assertEquals(current.id, superseded.id)
        assertEquals("2026-01-27T00:00:00Z", superseded.validTo)
        // Original is untouched (data class copy semantics)
        assertNull(current.validTo)
    }
}
