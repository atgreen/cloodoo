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
        val zdt = ZonedDateTime.parse(dateStr)
        zdt.toInstant().toEpochMilli()
    } catch (e: Exception) {
        null
    }
}

fun millisToIsoDate(millis: Long): String {
    // DatePicker returns UTC midnight for the selected date
    // We need to interpret this as a date, not convert from UTC
    val instant = Instant.ofEpochMilli(millis)
    val localDate = instant.atZone(ZoneId.of("UTC")).toLocalDate()
    val zdt = localDate.atStartOfDay(ZoneId.systemDefault())
    return zdt.format(DateTimeFormatter.ISO_OFFSET_DATE_TIME)
}
