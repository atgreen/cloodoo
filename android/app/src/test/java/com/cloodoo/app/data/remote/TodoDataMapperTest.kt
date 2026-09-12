// TodoDataMapperTest.kt
//
// SPDX-License-Identifier: MIT
//
// Copyright (C) 2026 Anthony Green <green@moxielogic.com>

package com.cloodoo.app.data.remote

import com.cloodoo.app.proto.CloodooSync.TodoData
import org.junit.Assert.assertEquals
import org.junit.Assert.assertNull
import org.junit.Test

/**
 * Tests for the TodoData proto → map conversion used for local storage.
 */
class TodoDataMapperTest {

    private fun fullTodo(): TodoData = TodoData.newBuilder()
        .setId("todo-1")
        .setTitle("Buy milk")
        .setDescription("2% if they have it")
        .setPriority("high")
        .setStatus("pending")
        .setScheduledDate("2026-01-26T00:00:00Z")
        .setDueDate("2026-01-27T00:00:00Z")
        .addAllTags(listOf("errand", "grocery"))
        .setUrl("https://example.com")
        .setCreatedAt("2026-01-25T12:00:00Z")
        .setCompletedAt("2026-01-26T09:00:00Z")
        .setParentId("todo-0")
        .setRepeatInterval(2)
        .setRepeatUnit("week")
        .build()

    @Test
    fun toMapCarriesAllPopulatedFields() {
        val map = fullTodo().toMap()

        assertEquals("todo-1", map["id"])
        assertEquals("Buy milk", map["title"])
        assertEquals("2% if they have it", map["description"])
        assertEquals("high", map["priority"])
        assertEquals("pending", map["status"])
        assertEquals("2026-01-26T00:00:00Z", map["scheduled_date"])
        assertEquals("2026-01-27T00:00:00Z", map["due_date"])
        assertEquals("errand,grocery", map["tags"])
        assertEquals("https://example.com", map["url"])
        assertEquals("2026-01-25T12:00:00Z", map["created_at"])
        assertEquals("2026-01-26T09:00:00Z", map["completed_at"])
        assertEquals("todo-0", map["parent_id"])
        assertEquals(2, map["repeat_interval"])
        assertEquals("week", map["repeat_unit"])
    }

    @Test
    fun toMapConvertsEmptyProtoStringsToNull() {
        val map = TodoData.newBuilder()
            .setId("todo-2")
            .setTitle("Minimal")
            .setPriority("medium")
            .setStatus("pending")
            .setCreatedAt("2026-01-25T12:00:00Z")
            .build()
            .toMap()

        assertNull(map["description"])
        assertNull(map["scheduled_date"])
        assertNull(map["due_date"])
        assertNull(map["tags"])
        assertNull(map["url"])
        assertNull(map["completed_at"])
        assertNull(map["parent_id"])
        assertNull(map["repeat_unit"])
    }

    @Test
    fun toMapConvertsZeroRepeatIntervalToNull() {
        val map = TodoData.newBuilder()
            .setId("todo-3")
            .setTitle("One-shot")
            .setCreatedAt("2026-01-25T12:00:00Z")
            .build()
            .toMap()

        assertNull(map["repeat_interval"])
    }

    @Test
    fun toMapJoinsSingleTagWithoutSeparator() {
        val map = TodoData.newBuilder()
            .setId("todo-4")
            .setTitle("Tagged")
            .addTags("solo")
            .setCreatedAt("2026-01-25T12:00:00Z")
            .build()
            .toMap()

        assertEquals("solo", map["tags"])
    }
}
