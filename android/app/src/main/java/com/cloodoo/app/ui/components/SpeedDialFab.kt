// SpeedDialFab.kt
//
// SPDX-License-Identifier: MIT
//
// Copyright (C) 2026 Anthony Green <green@moxielogic.com>

package com.cloodoo.app.ui.components

import androidx.compose.animation.AnimatedVisibility
import androidx.compose.animation.core.animateFloatAsState
import androidx.compose.animation.core.tween
import androidx.compose.animation.fadeIn
import androidx.compose.animation.fadeOut
import androidx.compose.animation.slideInVertically
import androidx.compose.animation.slideOutVertically
import androidx.compose.foundation.background
import androidx.compose.foundation.clickable
import androidx.compose.foundation.interaction.MutableInteractionSource
import androidx.compose.foundation.layout.*
import androidx.compose.material.icons.Icons
import androidx.compose.material.icons.filled.Add
import androidx.compose.material.icons.filled.CameraAlt
import androidx.compose.material.icons.filled.Edit
import androidx.compose.material.icons.filled.FlashOn
import androidx.compose.material.icons.filled.Mic
import androidx.compose.material3.*
import androidx.compose.runtime.*
import androidx.compose.ui.Alignment
import androidx.compose.ui.Modifier
import androidx.compose.ui.draw.rotate
import androidx.compose.ui.graphics.Color
import androidx.compose.ui.graphics.vector.ImageVector
import androidx.compose.ui.unit.dp

@Composable
fun SpeedDialFab(
    onTypeClick: () -> Unit,
    onSpeakClick: () -> Unit,
    onCameraClick: () -> Unit,
    onQuickAddClick: () -> Unit,
) {
    var expanded by remember { mutableStateOf(false) }
    val rotation by animateFloatAsState(
        targetValue = if (expanded) 45f else 0f,
        animationSpec = tween(durationMillis = 200),
        label = "fab_rotation"
    )

    Box(modifier = Modifier.fillMaxSize()) {
        // Scrim overlay
        AnimatedVisibility(
            visible = expanded,
            enter = fadeIn(tween(200)),
            exit = fadeOut(tween(200)),
            modifier = Modifier.fillMaxSize()
        ) {
            Box(
                modifier = Modifier
                    .fillMaxSize()
                    .background(Color.Black.copy(alpha = 0.4f))
                    .clickable(
                        interactionSource = remember { MutableInteractionSource() },
                        indication = null
                    ) { expanded = false }
            )
        }

        // FAB column
        Column(
            horizontalAlignment = Alignment.End,
            verticalArrangement = Arrangement.spacedBy(12.dp),
            modifier = Modifier
                .align(Alignment.BottomEnd)
                .padding(end = 16.dp, bottom = 16.dp)
        ) {
            // Mini-FABs (bottom to top, closest to FAB first)
            AnimatedVisibility(
                visible = expanded,
                enter = fadeIn(tween(150)) + slideInVertically(
                    initialOffsetY = { it / 2 },
                    animationSpec = tween(200)
                ),
                exit = fadeOut(tween(100)) + slideOutVertically(
                    targetOffsetY = { it / 2 },
                    animationSpec = tween(150)
                )
            ) {
                Column(
                    horizontalAlignment = Alignment.End,
                    verticalArrangement = Arrangement.spacedBy(12.dp)
                ) {
                    MiniFabWithLabel(
                        label = "Scan",
                        icon = Icons.Default.CameraAlt,
                        contentDescription = "OCR capture",
                        onClick = {
                            expanded = false
                            onCameraClick()
                        }
                    )
                    MiniFabWithLabel(
                        label = "Speak",
                        icon = Icons.Default.Mic,
                        contentDescription = "Voice add",
                        onClick = {
                            expanded = false
                            onSpeakClick()
                        }
                    )
                    MiniFabWithLabel(
                        label = "Form",
                        icon = Icons.Default.Edit,
                        contentDescription = "Form add",
                        onClick = {
                            expanded = false
                            onTypeClick()
                        }
                    )
                    MiniFabWithLabel(
                        label = "Quick",
                        icon = Icons.Default.FlashOn,
                        contentDescription = "Quick add",
                        onClick = {
                            expanded = false
                            onQuickAddClick()
                        }
                    )
                }
            }

            // Main FAB
            FloatingActionButton(
                onClick = { expanded = !expanded }
            ) {
                Icon(
                    Icons.Default.Add,
                    contentDescription = if (expanded) "Close" else "Add Task",
                    modifier = Modifier.rotate(rotation)
                )
            }
        }
    }
}

@Composable
private fun MiniFabWithLabel(
    label: String,
    icon: ImageVector,
    contentDescription: String,
    onClick: () -> Unit,
) {
    Row(
        verticalAlignment = Alignment.CenterVertically,
        horizontalArrangement = Arrangement.spacedBy(8.dp)
    ) {
        Surface(
            shape = MaterialTheme.shapes.small,
            tonalElevation = 2.dp,
            shadowElevation = 2.dp,
        ) {
            Text(
                text = label,
                modifier = Modifier.padding(horizontal = 8.dp, vertical = 4.dp),
                style = MaterialTheme.typography.labelMedium
            )
        }
        SmallFloatingActionButton(onClick = onClick) {
            Icon(icon, contentDescription = contentDescription)
        }
    }
}
