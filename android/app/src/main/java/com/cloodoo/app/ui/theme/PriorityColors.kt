// PriorityColors.kt
//
// SPDX-License-Identifier: MIT
//
// Copyright (C) 2026 Anthony Green <green@moxielogic.com>

package com.cloodoo.app.ui.theme

import androidx.compose.material3.MaterialTheme
import androidx.compose.runtime.Composable
import androidx.compose.runtime.CompositionLocalProvider
import androidx.compose.runtime.Immutable
import androidx.compose.runtime.staticCompositionLocalOf
import androidx.compose.ui.graphics.Color

@Immutable
data class PriorityColorScheme(
    val high: Color,
    val medium: Color,
    val low: Color,
    val none: Color,
    val highContainer: Color,
    val mediumContainer: Color,
    val lowContainer: Color
)

val LocalPriorityColors = staticCompositionLocalOf {
    PriorityColorScheme(
        high = Color(0xFFE53935),
        medium = Color(0xFFFB8C00),
        low = Color(0xFF43A047),
        none = Color.Gray,
        highContainer = Color(0xFFFFDAD6),
        mediumContainer = Color(0xFFFFE0B2),
        lowContainer = Color(0xFFC8E6C9)
    )
}

@Composable
fun ProvidePriorityColors(content: @Composable () -> Unit) {
    val colors = PriorityColorScheme(
        high = MaterialTheme.colorScheme.error,
        medium = MaterialTheme.colorScheme.tertiary,
        low = MaterialTheme.colorScheme.secondary,
        none = MaterialTheme.colorScheme.outline,
        highContainer = MaterialTheme.colorScheme.errorContainer,
        mediumContainer = MaterialTheme.colorScheme.tertiaryContainer,
        lowContainer = MaterialTheme.colorScheme.secondaryContainer
    )
    CompositionLocalProvider(LocalPriorityColors provides colors) {
        content()
    }
}
