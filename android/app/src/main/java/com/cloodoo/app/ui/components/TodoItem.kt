package com.cloodoo.app.ui.components

import androidx.compose.animation.animateColorAsState
import androidx.compose.animation.core.Animatable
import androidx.compose.animation.core.animateFloatAsState
import androidx.compose.animation.core.tween
import androidx.compose.foundation.background
import androidx.compose.foundation.clickable
import androidx.compose.foundation.gestures.detectHorizontalDragGestures
import androidx.compose.foundation.layout.*
import androidx.compose.foundation.shape.CircleShape
import androidx.compose.foundation.shape.RoundedCornerShape
import androidx.compose.material.icons.Icons
import androidx.compose.material.icons.filled.CheckCircle
import androidx.compose.material.icons.filled.Delete
import androidx.compose.material.icons.filled.Done
import androidx.compose.material.icons.filled.Undo
import androidx.compose.material.icons.outlined.AccessTime
import androidx.compose.material.icons.outlined.Circle
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
import com.cloodoo.app.data.local.TodoEntity
import com.google.gson.Gson
import com.google.gson.reflect.TypeToken
import kotlinx.coroutines.launch
import java.time.LocalDate
import java.time.ZonedDateTime
import java.time.format.DateTimeFormatter
import kotlin.math.roundToInt

/**
 * Priority colors for the left border accent
 */
private fun getPriorityColor(priority: String): Color {
    return when (priority.lowercase()) {
        "high" -> Color(0xFFE53935)   // Red
        "medium" -> Color(0xFFFB8C00) // Orange
        "low" -> Color(0xFF43A047)    // Green
        else -> Color.Gray
    }
}

/**
 * Enhanced TODO item with left priority border and rich metadata display
 */
@Composable
fun TodoItem(
    todo: TodoEntity,
    onToggleComplete: (String) -> Unit,
    onClick: (String) -> Unit,
    modifier: Modifier = Modifier
) {
    val isCompleted = todo.status == "completed"
    val priorityColor = getPriorityColor(todo.priority)

    Card(
        modifier = modifier
            .fillMaxWidth()
            .padding(horizontal = 12.dp, vertical = 3.dp)
            .clickable { onClick(todo.id) },
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
        Row(modifier = Modifier.fillMaxWidth()) {
            // Priority color bar on left edge
            Box(
                modifier = Modifier
                    .width(4.dp)
                    .fillMaxHeight()
                    .background(
                        if (isCompleted) priorityColor.copy(alpha = 0.3f)
                        else priorityColor
                    )
            )

            Row(
                modifier = Modifier
                    .fillMaxWidth()
                    .padding(start = 8.dp, end = 12.dp, top = 10.dp, bottom = 10.dp),
                verticalAlignment = Alignment.Top
            ) {
                // Completion checkbox
                IconButton(
                    onClick = { onToggleComplete(todo.id) },
                    modifier = Modifier.size(32.dp)
                ) {
                    Icon(
                        imageVector = if (isCompleted) Icons.Filled.CheckCircle else Icons.Outlined.Circle,
                        contentDescription = if (isCompleted) "Mark incomplete" else "Mark complete",
                        tint = if (isCompleted) {
                            MaterialTheme.colorScheme.secondary
                        } else {
                            priorityColor.copy(alpha = 0.7f)
                        },
                        modifier = Modifier.size(24.dp)
                    )
                }

                Spacer(modifier = Modifier.width(4.dp))

                Column(modifier = Modifier.weight(1f)) {
                    // Title
                    Text(
                        text = todo.title,
                        style = MaterialTheme.typography.bodyLarge,
                        color = if (isCompleted) {
                            MaterialTheme.colorScheme.onSurfaceVariant.copy(alpha = 0.6f)
                        } else {
                            MaterialTheme.colorScheme.onSurface
                        },
                        textDecoration = if (isCompleted) TextDecoration.LineThrough else null,
                        maxLines = 2,
                        overflow = TextOverflow.Ellipsis
                    )

                    // Metadata row: due date, time estimate, location
                    val hasMetadata = todo.dueDate != null ||
                                      todo.estimatedMinutes != null ||
                                      todo.locationInfo != null

                    if (hasMetadata && !isCompleted) {
                        Spacer(modifier = Modifier.height(4.dp))
                        Row(
                            verticalAlignment = Alignment.CenterVertically,
                            horizontalArrangement = Arrangement.spacedBy(8.dp)
                        ) {
                            // Due date chip
                            todo.dueDate?.let { dateStr ->
                                DueDateChip(dateStr = dateStr)
                            }

                            // Estimated time
                            todo.estimatedMinutes?.let { minutes ->
                                if (minutes > 0) {
                                    MetadataChip(
                                        icon = Icons.Outlined.AccessTime,
                                        text = formatDuration(minutes)
                                    )
                                }
                            }

                            // Location indicator
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
                        }
                    }

                    // Tags row
                    val tags = parseTags(todo.tags)
                    if (tags.isNotEmpty() && !isCompleted) {
                        Spacer(modifier = Modifier.height(6.dp))
                        Row(
                            horizontalArrangement = Arrangement.spacedBy(4.dp)
                        ) {
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
}

/**
 * Swipe-to-reveal TODO item - swipe left to reveal action buttons
 */
@Composable
fun SwipeableTodoItem(
    todo: TodoEntity,
    onToggleComplete: (String) -> Unit,
    onClick: (String) -> Unit,
    onDelete: (String) -> Unit,
    modifier: Modifier = Modifier
) {
    val isCompleted = todo.status == "completed"
    val scope = rememberCoroutineScope()

    // Width of revealed action buttons area
    val actionButtonsWidth = 140.dp
    val actionButtonsWidthPx = with(LocalDensity.current) { actionButtonsWidth.toPx() }

    // Swipe offset - negative means swiped left (revealing right-side buttons)
    val offsetX = remember { Animatable(0f) }

    Box(
        modifier = modifier
            .fillMaxWidth()
            .padding(horizontal = 12.dp, vertical = 3.dp)
            .clip(RoundedCornerShape(8.dp))
    ) {
        // Background action buttons (revealed when swiping left)
        Row(
            modifier = Modifier
                .fillMaxHeight()
                .width(actionButtonsWidth)
                .align(Alignment.CenterEnd),
            horizontalArrangement = Arrangement.End
        ) {
            // Complete/Undo button
            Box(
                modifier = Modifier
                    .fillMaxHeight()
                    .weight(1f)
                    .background(if (isCompleted) Color(0xFFFFA726) else Color(0xFF66BB6A))
                    .clickable {
                        scope.launch {
                            offsetX.animateTo(0f, tween(200))
                        }
                        onToggleComplete(todo.id)
                    },
                contentAlignment = Alignment.Center
            ) {
                Column(
                    horizontalAlignment = Alignment.CenterHorizontally
                ) {
                    Icon(
                        imageVector = if (isCompleted) Icons.Default.Undo else Icons.Default.Done,
                        contentDescription = if (isCompleted) "Reopen" else "Complete",
                        tint = Color.White,
                        modifier = Modifier.size(24.dp)
                    )
                    Spacer(modifier = Modifier.height(2.dp))
                    Text(
                        text = if (isCompleted) "Undo" else "Done",
                        style = MaterialTheme.typography.labelSmall,
                        color = Color.White
                    )
                }
            }

            // Delete button
            Box(
                modifier = Modifier
                    .fillMaxHeight()
                    .weight(1f)
                    .background(Color(0xFFEF5350))
                    .clickable {
                        scope.launch {
                            offsetX.animateTo(0f, tween(200))
                        }
                        onDelete(todo.id)
                    },
                contentAlignment = Alignment.Center
            ) {
                Column(
                    horizontalAlignment = Alignment.CenterHorizontally
                ) {
                    Icon(
                        imageVector = Icons.Default.Delete,
                        contentDescription = "Delete",
                        tint = Color.White,
                        modifier = Modifier.size(24.dp)
                    )
                    Spacer(modifier = Modifier.height(2.dp))
                    Text(
                        text = "Delete",
                        style = MaterialTheme.typography.labelSmall,
                        color = Color.White
                    )
                }
            }
        }

        // Foreground card (slides left to reveal buttons)
        Box(
            modifier = Modifier
                .fillMaxWidth()
                .offset { IntOffset(offsetX.value.roundToInt(), 0) }
                .pointerInput(Unit) {
                    detectHorizontalDragGestures(
                        onDragEnd = {
                            scope.launch {
                                // Snap to revealed or closed position
                                val target = if (offsetX.value < -actionButtonsWidthPx / 2) {
                                    -actionButtonsWidthPx  // Reveal buttons
                                } else {
                                    0f  // Close
                                }
                                offsetX.animateTo(target, tween(200))
                            }
                        },
                        onHorizontalDrag = { _, dragAmount ->
                            scope.launch {
                                val newOffset = (offsetX.value + dragAmount)
                                    .coerceIn(-actionButtonsWidthPx, 0f)  // Only allow left swipe
                                offsetX.snapTo(newOffset)
                            }
                        }
                    )
                }
        ) {
            TodoItem(
                todo = todo,
                onToggleComplete = onToggleComplete,
                onClick = onClick
            )
        }
    }
}

@Composable
fun PriorityBadge(priority: String, modifier: Modifier = Modifier) {
    val (color, label) = when (priority.lowercase()) {
        "high" -> Pair(Color(0xFFE53935), "H")
        "medium" -> Pair(Color(0xFFFB8C00), "M")
        "low" -> Pair(Color(0xFF43A047), "L")
        else -> Pair(Color.Gray, "?")
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

/**
 * Parse tags from JSON array string
 */
private fun parseTags(tagsJson: String?): List<String> {
    if (tagsJson.isNullOrBlank() || tagsJson == "null") return emptyList()
    return try {
        val type = object : TypeToken<List<String>>() {}.type
        Gson().fromJson(tagsJson, type) ?: emptyList()
    } catch (e: Exception) {
        emptyList()
    }
}

/**
 * Parse location name from JSON object string
 */
private fun parseLocationName(locationJson: String): String? {
    return try {
        val type = object : TypeToken<Map<String, Any?>>() {}.type
        val map: Map<String, Any?> = Gson().fromJson(locationJson, type)
        map["name"] as? String
    } catch (e: Exception) {
        null
    }
}

/**
 * Format duration in minutes to human readable string
 */
private fun formatDuration(minutes: Int): String {
    return when {
        minutes < 60 -> "${minutes}m"
        minutes % 60 == 0 -> "${minutes / 60}h"
        else -> "${minutes / 60}h ${minutes % 60}m"
    }
}
