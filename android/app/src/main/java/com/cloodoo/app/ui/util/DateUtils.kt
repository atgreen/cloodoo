// DateUtils.kt
//
// SPDX-License-Identifier: MIT
//
// Copyright (C) 2026 Anthony Green <green@moxielogic.com>

package com.cloodoo.app.ui.util

import java.time.Instant
import java.time.ZoneId
import java.time.ZonedDateTime
import java.time.format.DateTimeFormatter

fun formatDate(dateStr: String): String {
    return try {
        val zdt = ZonedDateTime.parse(dateStr)
        val formatter = DateTimeFormatter.ofPattern("EEE, MMM d, yyyy")
        zdt.format(formatter)
    } catch (e: Exception) {
        dateStr
    }
}

fun parseDateToMillis(dateStr: String): Long? {
    return try {
        // Parse the ISO date string and extract just the date part
        val zdt = ZonedDateTime.parse(dateStr)
        val localDate = zdt.toLocalDate()

        // DatePicker expects UTC midnight, so convert the date to UTC midnight millis
        localDate.atStartOfDay(ZoneId.of("UTC")).toInstant().toEpochMilli()
    } catch (e: Exception) {
        null
    }
}

fun millisToIsoDate(millis: Long): String {
    // Material3 DatePicker returns millis representing the selected date at UTC midnight.
    // We want to save this as the same calendar date at local midnight.
    // Extract the date components from UTC and create local midnight from those.
    val utcDate = Instant.ofEpochMilli(millis)
        .atZone(ZoneId.of("UTC"))
        .toLocalDate()

    // Create a ZonedDateTime for that date at local midnight
    val localMidnight = utcDate.atStartOfDay(ZoneId.systemDefault())
    return localMidnight.format(DateTimeFormatter.ISO_OFFSET_DATE_TIME)
}
