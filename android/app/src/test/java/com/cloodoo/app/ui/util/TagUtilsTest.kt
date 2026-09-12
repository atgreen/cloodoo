// TagUtilsTest.kt
//
// SPDX-License-Identifier: MIT
//
// Copyright (C) 2026 Anthony Green <green@moxielogic.com>

package com.cloodoo.app.ui.util

import org.junit.Assert.assertEquals
import org.junit.Assert.assertTrue
import org.junit.Test

class TagUtilsTest {

    @Test
    fun parseTagsReturnsEmptyListForNull() {
        assertTrue(parseTags(null).isEmpty())
    }

    @Test
    fun parseTagsReturnsEmptyListForBlank() {
        assertTrue(parseTags("").isEmpty())
        assertTrue(parseTags("   ").isEmpty())
    }

    @Test
    fun parseTagsReturnsEmptyListForLiteralNullString() {
        assertTrue(parseTags("null").isEmpty())
    }

    @Test
    fun parseTagsSplitsCommaSeparatedValues() {
        assertEquals(listOf("work", "urgent"), parseTags("work,urgent"))
    }

    @Test
    fun parseTagsTrimsWhitespaceAroundEntries() {
        assertEquals(listOf("work", "urgent"), parseTags(" work , urgent "))
    }

    @Test
    fun parseTagsDropsEmptyEntries() {
        assertEquals(listOf("work", "urgent"), parseTags("work,,urgent,"))
    }

    @Test
    fun parseTagsParsesJsonArrayFormat() {
        assertEquals(listOf("work", "urgent"), parseTags("""["work","urgent"]"""))
    }

    @Test
    fun parseTagsParsesJsonArrayWithLeadingWhitespace() {
        assertEquals(listOf("home"), parseTags("""  ["home"]"""))
    }

    @Test
    fun parseTagsReturnsEmptyListForEmptyJsonArray() {
        assertTrue(parseTags("[]").isEmpty())
    }

    @Test
    fun parseTagsFallsBackToCommaSplitOnMalformedJson() {
        // Starts with "[" but is not valid JSON; falls back to comma splitting
        assertEquals(listOf("[oops", "tag"), parseTags("[oops,tag"))
    }

    @Test
    fun parseTagsHandlesSingleTag() {
        assertEquals(listOf("solo"), parseTags("solo"))
    }

    @Test
    fun tagsToStorageStringJoinsWithCommas() {
        assertEquals("work,urgent", tagsToStorageString(listOf("work", "urgent")))
    }

    @Test
    fun tagsToStorageStringOfEmptyListIsEmpty() {
        assertEquals("", tagsToStorageString(emptyList()))
    }

    @Test
    fun tagsRoundTripThroughStorageString() {
        val tags = listOf("work", "urgent", "home")
        assertEquals(tags, parseTags(tagsToStorageString(tags)))
    }
}
