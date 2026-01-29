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
    val instant = Instant.ofEpochMilli(millis)
    val zdt = ZonedDateTime.ofInstant(instant, ZoneId.systemDefault())
    return zdt.format(DateTimeFormatter.ISO_OFFSET_DATE_TIME)
}
