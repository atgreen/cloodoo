// DateUtilsTest.kt
//
// SPDX-License-Identifier: MIT
//
// Copyright (C) 2026 Anthony Green <green@moxielogic.com>

package com.cloodoo.app.ui.util

import org.junit.After
import org.junit.Assert.assertEquals
import org.junit.Assert.assertNull
import org.junit.Before
import org.junit.Test
import java.time.ZonedDateTime
import java.util.Locale
import java.util.TimeZone

class DateUtilsTest {

    private lateinit var originalLocale: Locale
    private lateinit var originalTimeZone: TimeZone

    @Before
    fun pinLocaleAndTimeZone() {
        originalLocale = Locale.getDefault()
        originalTimeZone = TimeZone.getDefault()
        Locale.setDefault(Locale.US)
        TimeZone.setDefault(TimeZone.getTimeZone("America/New_York"))
    }

    @After
    fun restoreLocaleAndTimeZone() {
        Locale.setDefault(originalLocale)
        TimeZone.setDefault(originalTimeZone)
    }

    // ── formatDate ──

    @Test
    fun formatDateRendersHumanReadableDate() {
        assertEquals("Mon, Jan 26, 2026", formatDate("2026-01-26T14:07:39Z"))
    }

    @Test
    fun formatDateKeepsDateFromOffsetTimestamp() {
        assertEquals("Sat, Mar 7, 2026", formatDate("2026-03-07T00:00:00-05:00"))
    }

    @Test
    fun formatDateReturnsInputWhenUnparseable() {
        assertEquals("not-a-date", formatDate("not-a-date"))
        assertEquals("", formatDate(""))
    }

    // ── parseDateToMillis ──

    @Test
    fun parseDateToMillisReturnsUtcMidnightOfCalendarDate() {
        // 2026-01-26 in any zone maps to 2026-01-26T00:00:00Z
        val expected = 1769385600000L
        assertEquals(expected, parseDateToMillis("2026-01-26T14:07:39Z"))
        assertEquals(expected, parseDateToMillis("2026-01-26T00:00:00-05:00"))
    }

    @Test
    fun parseDateToMillisReturnsNullWhenUnparseable() {
        assertNull(parseDateToMillis("garbage"))
        assertNull(parseDateToMillis(""))
    }

    // ── millisToIsoDate ──

    @Test
    fun millisToIsoDateProducesLocalMidnightOfSameCalendarDate() {
        // UTC midnight of 2026-01-26 becomes local (America/New_York) midnight
        val result = ZonedDateTime.parse(millisToIsoDate(1769385600000L))
        assertEquals(2026, result.year)
        assertEquals(1, result.monthValue)
        assertEquals(26, result.dayOfMonth)
        assertEquals(0, result.hour)
        assertEquals(0, result.minute)
    }

    @Test
    fun millisRoundTripPreservesCalendarDate() {
        // DatePicker hands back UTC midnight; saving and re-parsing must keep the date
        val picked = parseDateToMillis("2026-07-04T09:30:00Z")!!
        val saved = millisToIsoDate(picked)
        assertEquals(picked, parseDateToMillis(saved))
    }
}
