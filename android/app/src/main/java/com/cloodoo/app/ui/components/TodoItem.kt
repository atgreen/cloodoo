// TodoItem.kt
//
// SPDX-License-Identifier: MIT
//
// Copyright (C) 2026 Anthony Green <green@moxielogic.com>

package com.cloodoo.app.ui.components

import androidx.compose.animation.animateColorAsState
import androidx.compose.animation.core.animateFloatAsState
import androidx.compose.animation.core.tween
import androidx.compose.foundation.background
import androidx.compose.foundation.clickable
import androidx.compose.foundation.ExperimentalFoundationApi
import androidx.compose.foundation.combinedClickable
import androidx.compose.foundation.gestures.detectHorizontalDragGestures
import androidx.compose.foundation.layout.*
import androidx.compose.foundation.shape.CircleShape
import androidx.compose.foundation.shape.RoundedCornerShape
import androidx.compose.material.icons.Icons
import androidx.compose.material.icons.filled.Close
import androidx.compose.material.icons.filled.Done
import androidx.compose.material.icons.filled.Photo
import androidx.compose.material.icons.filled.Repeat
import androidx.compose.material.icons.automirrored.filled.Undo
import androidx.compose.material.icons.outlined.Place
import androidx.compose.material3.*
import androidx.compose.runtime.*
import androidx.compose.ui.Alignment
import androidx.compose.ui.Modifier
import androidx.compose.ui.draw.clip
import androidx.compose.ui.draw.scale
import androidx.compose.ui.graphics.Color
import androidx.compose.ui.input.pointer.pointerInput
import androidx.compose.ui.platform.LocalDensity
import androidx.compose.ui.text.style.TextDecoration
import androidx.compose.ui.text.style.TextOverflow
import androidx.compose.ui.unit.IntOffset
import androidx.compose.ui.unit.dp
import kotlin.math.roundToInt
import com.cloodoo.app.data.local.TodoEntity
import com.cloodoo.app.ui.theme.LocalPriorityColors
import com.cloodoo.app.ui.theme.PriorityColorScheme
import com.cloodoo.app.ui.util.parseTags
import com.google.gson.Gson
import com.google.gson.reflect.TypeToken
import java.time.DayOfWeek
import java.time.LocalDate
import java.time.ZonedDateTime
import java.time.format.DateTimeFormatter
import java.time.temporal.TemporalAdjusters

private fun getPriorityColor(priority: String, colors: PriorityColorScheme): Color {
    return when (priority.lowercase()) {
        "high" -> colors.high
        "medium" -> colors.medium
        "low" -> colors.low
        else -> colors.none
    }
}

@Composable
fun TodoItem(
    todo: TodoEntity,
    onClick: ((String) -> Unit)? = null,
    onAttachmentClick: ((List<String>) -> Unit)? = null,
    modifier: Modifier = Modifier
) {
    val isCompleted = todo.status == "completed"
    val priorityColors = LocalPriorityColors.current
    val priorityColor = getPriorityColor(todo.priority, priorityColors)

    Card(
        modifier = modifier
            .fillMaxWidth()
            .then(if (onClick != null) Modifier.clickable { onClick(todo.id) } else Modifier),
        shape = RoundedCornerShape(8.dp),
        colors = CardDefaults.cardColors(
            containerColor = if (isCompleted) {
                MaterialTheme.colorScheme.surfaceVariant.copy(alpha = 0.5f)
            } else {
                MaterialTheme.colorScheme.surface
            }
        ),
        elevation = CardDefaults.cardElevation(
            defaultElevation = if (isCompleted) 0.dp else 1.dp
        )
    ) {
        Row(
            modifier = Modifier
                .fillMaxWidth()
                .height(IntrinsicSize.Min)
        ) {
            // Priority accent strip
            Box(
                modifier = Modifier
                    .width(12.dp)
                    .fillMaxHeight()
                    .background(
                        if (isCompleted) priorityColor.copy(alpha = 0.3f)
                        else priorityColor
                    )
            )

            Column(
                modifier = Modifier
                    .fillMaxWidth()
                    .padding(horizontal = 10.dp, vertical = 8.dp)
            ) {
                Text(
                    text = todo.title,
                    style = MaterialTheme.typography.bodyMedium,
                    color = if (isCompleted) {
                        MaterialTheme.colorScheme.onSurfaceVariant.copy(alpha = 0.6f)
                    } else {
                        MaterialTheme.colorScheme.onSurface
                    },
                    textDecoration = if (isCompleted) TextDecoration.LineThrough else null,
                    maxLines = 2,
                    overflow = TextOverflow.Ellipsis
                )

                // Dates, location, repeat, attachments, and tags on one line
                val tags = parseTags(todo.tags)
                val hasRepeat = todo.repeatInterval != null && todo.repeatInterval > 0 && !todo.repeatUnit.isNullOrEmpty()
                val attachmentCount = parseAttachmentCount(todo.attachmentHashes)
                val hasAttachments = attachmentCount > 0
                val hasMetadata = todo.dueDate != null || todo.locationInfo != null || tags.isNotEmpty() || hasRepeat || hasAttachments
                if (hasMetadata && !isCompleted) {
                    Spacer(modifier = Modifier.height(3.dp))
                    Row(
                        verticalAlignment = Alignment.CenterVertically,
                        horizontalArrangement = Arrangement.spacedBy(6.dp)
                    ) {
                        if (hasRepeat) {
                            val repeatLabel = when {
                                todo.repeatInterval == 1 -> when (todo.repeatUnit) {
                                    "day" -> "Daily"
                                    "week" -> "Weekly"
                                    "month" -> "Monthly"
                                    "year" -> "Yearly"
                                    else -> ""
                                }
                                else -> "Every ${todo.repeatInterval} ${todo.repeatUnit}s"
                            }
                            MetadataChip(
                                icon = Icons.Default.Repeat,
                                text = repeatLabel
                            )
                        }
                        if (hasAttachments) {
                            val hashes = parseAttachmentHashes(todo.attachmentHashes)
                            Box(
                                modifier = if (onAttachmentClick != null) {
                                    Modifier.clickable { onAttachmentClick(hashes) }
                                } else Modifier
                            ) {
                                MetadataChip(
                                    icon = Icons.Default.Photo,
                                    text = if (attachmentCount == 1) "1 photo" else "$attachmentCount photos"
                                )
                            }
                        }
                        todo.dueDate?.let { dateStr ->
                            DueDateChip(dateStr = dateStr)
                        }
                        todo.locationInfo?.let { locationJson ->
                            if (locationJson.isNotBlank() && locationJson != "null") {
                                val locationName = parseLocationName(locationJson)
                                if (locationName != null) {
                                    MetadataChip(
                                        icon = Icons.Outlined.Place,
                                        text = locationName,
                                        maxWidth = 100.dp
                                    )
                                }
                            }
                        }
                        tags.take(3).forEach { tag ->
                            TagChip(tag = tag)
                        }
                        if (tags.size > 3) {
                            TagChip(tag = "+${tags.size - 3}")
                        }
                    }
                }
            }
        }
    }
}

@OptIn(ExperimentalFoundationApi::class, ExperimentalMaterial3Api::class)
@Composable
fun SwipeableTodoItem(
    todo: TodoEntity,
    onToggleComplete: (String) -> Unit,
    onClick: (String) -> Unit,
    onCancel: (String) -> Unit,
    onPostpone: ((String, PostponeOption) -> Unit)? = null,
    onAttachmentClick: ((List<String>) -> Unit)? = null,
    enableCancel: Boolean = true,
    modifier: Modifier = Modifier
) {
    var showPostponeSheet by remember { mutableStateOf(false) }
    val isDone = todo.status == "completed" || todo.status == "cancelled"
    val density = LocalDensity.current
    val swipeThreshold = with(density) { 100.dp.toPx() }

    var offsetX by remember { mutableFloatStateOf(0f) }
    var isDragging by remember { mutableStateOf(false) }

    // Animate back to zero when released
    val animatedOffset by animateFloatAsState(
        targetValue = if (isDragging) offsetX else 0f,
        animationSpec = tween(durationMillis = 300),
        label = "swipe_offset"
    )

    // Determine background state
    val showingRight = animatedOffset > 10f
    val showingLeft = animatedOffset < -10f

    val backgroundColor by animateColorAsState(
        targetValue = when {
            showingRight -> if (isDone) MaterialTheme.colorScheme.tertiary else MaterialTheme.colorScheme.secondary
            showingLeft && enableCancel -> MaterialTheme.colorScheme.error
            else -> Color.Transparent
        },
        label = "swipe_bg_color"
    )

    val icon = when {
        showingRight -> if (isDone) Icons.AutoMirrored.Filled.Undo else Icons.Default.Done
        showingLeft -> Icons.Default.Close
        else -> Icons.Default.Done
    }

    val iconAlignment = when {
        showingRight -> Alignment.CenterStart
        showingLeft -> Alignment.CenterEnd
        else -> Alignment.Center
    }

    val iconScale by animateFloatAsState(
        targetValue = if (!showingRight && !showingLeft) 0.5f else 1f,
        label = "icon_scale"
    )

    Box(
        modifier = modifier
            .padding(horizontal = 12.dp, vertical = 3.dp)
            .fillMaxWidth()
    ) {
        // Background layer
        Box(
            modifier = Modifier
                .matchParentSize()
                .background(backgroundColor, RoundedCornerShape(8.dp))
                .padding(horizontal = 24.dp),
            contentAlignment = iconAlignment
        ) {
            Icon(
                imageVector = icon,
                contentDescription = null,
                modifier = Modifier.scale(iconScale),
                tint = Color.White
            )
        }

        // Foreground TodoItem with drag gesture
        Box(
            modifier = Modifier
                .offset { IntOffset(animatedOffset.roundToInt(), 0) }
                .pointerInput(Unit) {
                    detectHorizontalDragGestures(
                        onDragStart = {
                            isDragging = true
                        },
                        onDragEnd = {
                            // Only trigger action if threshold is reached (position-based only)
                            when {
                                offsetX >= swipeThreshold -> onToggleComplete(todo.id)
                                enableCancel && offsetX <= -swipeThreshold -> onCancel(todo.id)
                            }
                            isDragging = false
                            offsetX = 0f
                        },
                        onDragCancel = {
                            isDragging = false
                            offsetX = 0f
                        },
                        onHorizontalDrag = { change, dragAmount ->
                            change.consume()
                            val newOffset = offsetX + dragAmount
                            // Allow dragging with resistance
                            offsetX = when {
                                newOffset > 0 -> newOffset.coerceAtMost(swipeThreshold * 1.3f)
                                newOffset < 0 && enableCancel -> newOffset.coerceAtLeast(-swipeThreshold * 1.3f)
                                else -> 0f
                            }
                        }
                    )
                }
                .combinedClickable(
                    onClick = { onClick(todo.id) },
                    onLongClick = {
                        if (onPostpone != null) {
                            showPostponeSheet = true
                        }
                    }
                )
        ) {
            TodoItem(
                todo = todo,
                onClick = null, // Click handled by combinedClickable above
                onAttachmentClick = onAttachmentClick
            )
        }
    }

    // Postpone bottom sheet
    if (showPostponeSheet && onPostpone != null) {
        PostponeBottomSheet(
            onDismiss = { showPostponeSheet = false },
            onSelect = { option ->
                onPostpone(todo.id, option)
                showPostponeSheet = false
            }
        )
    }
}

@Composable
fun PriorityBadge(priority: String, modifier: Modifier = Modifier) {
    val priorityColors = LocalPriorityColors.current
    val (color, label) = when (priority.lowercase()) {
        "high" -> Pair(priorityColors.high, "H")
        "medium" -> Pair(priorityColors.medium, "M")
        "low" -> Pair(priorityColors.low, "L")
        else -> Pair(priorityColors.none, "?")
    }

    Box(
        modifier = modifier
            .size(20.dp)
            .clip(CircleShape)
            .background(color),
        contentAlignment = Alignment.Center
    ) {
        Text(
            text = label,
            style = MaterialTheme.typography.labelSmall,
            color = Color.White
        )
    }
}

@Composable
fun DueDateChip(dateStr: String, modifier: Modifier = Modifier) {
    val (displayText, isOverdue, isToday) = try {
        val date = ZonedDateTime.parse(dateStr).toLocalDate()
        val today = LocalDate.now()
        Triple(
            when {
                date == today -> "Today"
                date == today.plusDays(1) -> "Tomorrow"
                date < today -> "Overdue"
                else -> date.format(DateTimeFormatter.ofPattern("MMM d"))
            },
            date < today,
            date == today
        )
    } catch (e: Exception) {
        Triple(dateStr.take(10), false, false)
    }

    Surface(
        modifier = modifier,
        shape = RoundedCornerShape(4.dp),
        color = when {
            isOverdue -> MaterialTheme.colorScheme.errorContainer
            isToday -> MaterialTheme.colorScheme.primaryContainer
            else -> MaterialTheme.colorScheme.surfaceVariant
        }
    ) {
        Text(
            text = displayText,
            style = MaterialTheme.typography.labelSmall,
            color = when {
                isOverdue -> MaterialTheme.colorScheme.onErrorContainer
                isToday -> MaterialTheme.colorScheme.onPrimaryContainer
                else -> MaterialTheme.colorScheme.onSurfaceVariant
            },
            modifier = Modifier.padding(horizontal = 6.dp, vertical = 2.dp)
        )
    }
}

@Composable
fun MetadataChip(
    icon: androidx.compose.ui.graphics.vector.ImageVector,
    text: String,
    modifier: Modifier = Modifier,
    maxWidth: androidx.compose.ui.unit.Dp = androidx.compose.ui.unit.Dp.Unspecified
) {
    Row(
        modifier = modifier
            .then(if (maxWidth != androidx.compose.ui.unit.Dp.Unspecified) Modifier.widthIn(max = maxWidth) else Modifier)
            .background(
                MaterialTheme.colorScheme.surfaceVariant,
                RoundedCornerShape(4.dp)
            )
            .padding(horizontal = 6.dp, vertical = 2.dp),
        verticalAlignment = Alignment.CenterVertically
    ) {
        Icon(
            imageVector = icon,
            contentDescription = null,
            modifier = Modifier.size(12.dp),
            tint = MaterialTheme.colorScheme.onSurfaceVariant
        )
        Spacer(modifier = Modifier.width(3.dp))
        Text(
            text = text,
            style = MaterialTheme.typography.labelSmall,
            color = MaterialTheme.colorScheme.onSurfaceVariant,
            maxLines = 1,
            overflow = TextOverflow.Ellipsis
        )
    }
}

@Composable
fun TagChip(tag: String, modifier: Modifier = Modifier) {
    Surface(
        modifier = modifier,
        shape = RoundedCornerShape(4.dp),
        color = MaterialTheme.colorScheme.secondaryContainer.copy(alpha = 0.7f)
    ) {
        Text(
            text = tag,
            style = MaterialTheme.typography.labelSmall,
            color = MaterialTheme.colorScheme.onSecondaryContainer,
            modifier = Modifier.padding(horizontal = 6.dp, vertical = 2.dp),
            maxLines = 1
        )
    }
}

private fun parseLocationName(locationJson: String): String? {
    return try {
        val type = object : TypeToken<Map<String, Any?>>() {}.type
        val map: Map<String, Any?> = Gson().fromJson(locationJson, type)
        map["name"] as? String
    } catch (e: Exception) {
        null
    }
}

private fun parseAttachmentCount(attachmentHashes: String?): Int {
    return parseAttachmentHashes(attachmentHashes).size
}

private fun parseAttachmentHashes(attachmentHashes: String?): List<String> {
    if (attachmentHashes.isNullOrBlank()) return emptyList()
    return try {
        val type = object : TypeToken<List<String>>() {}.type
        Gson().fromJson(attachmentHashes, type)
    } catch (e: Exception) {
        emptyList()
    }
}

/**
 * Options for postponing a task
 */
enum class PostponeOption(val label: String, val icon: androidx.compose.ui.graphics.vector.ImageVector) {
    TOMORROW("Tomorrow", Icons.Default.Done),
    NEXT_WEEK("Next Week", Icons.Default.Done),
    NEXT_MONDAY("Next Monday", Icons.Default.Done);

    fun calculateNewDate(currentDate: LocalDate = LocalDate.now()): LocalDate {
        return when (this) {
            TOMORROW -> currentDate.plusDays(1)
            NEXT_WEEK -> currentDate.plusWeeks(1)
            NEXT_MONDAY -> currentDate.with(TemporalAdjusters.next(DayOfWeek.MONDAY))
        }
    }
}

@OptIn(ExperimentalMaterial3Api::class)
@Composable
fun PostponeBottomSheet(
    onDismiss: () -> Unit,
    onSelect: (PostponeOption) -> Unit
) {
    val sheetState = rememberModalBottomSheetState()

    ModalBottomSheet(
        onDismissRequest = onDismiss,
        sheetState = sheetState
    ) {
        Column(
            modifier = Modifier
                .fillMaxWidth()
                .padding(bottom = 32.dp)
        ) {
            Text(
                text = "Postpone Task",
                style = MaterialTheme.typography.titleMedium,
                modifier = Modifier.padding(horizontal = 24.dp, vertical = 16.dp)
            )

            val today = LocalDate.now()

            PostponeOption.entries.forEach { option ->
                val targetDate = option.calculateNewDate(today)
                val dateLabel = when {
                    targetDate == today.plusDays(1) -> "Tomorrow"
                    else -> targetDate.format(DateTimeFormatter.ofPattern("EEE, MMM d"))
                }

                ListItem(
                    headlineContent = { Text(option.label) },
                    supportingContent = { Text(dateLabel) },
                    modifier = Modifier.clickable { onSelect(option) }
                )
            }
        }
    }
}
