package com.cloodoo.app.ui.components

import androidx.compose.foundation.background
import androidx.compose.foundation.clickable
import androidx.compose.foundation.layout.*
import androidx.compose.foundation.shape.CircleShape
import androidx.compose.foundation.shape.RoundedCornerShape
import androidx.compose.material.icons.Icons
import androidx.compose.material.icons.filled.CheckCircle
import androidx.compose.material.icons.outlined.Circle
import androidx.compose.material3.*
import androidx.compose.runtime.Composable
import androidx.compose.ui.Alignment
import androidx.compose.ui.Modifier
import androidx.compose.ui.draw.clip
import androidx.compose.ui.graphics.Color
import androidx.compose.ui.text.style.TextDecoration
import androidx.compose.ui.text.style.TextOverflow
import androidx.compose.ui.unit.dp
import com.cloodoo.app.data.local.TodoEntity
import java.time.LocalDate
import java.time.ZonedDateTime
import java.time.format.DateTimeFormatter

@Composable
fun TodoItem(
    todo: TodoEntity,
    onToggleComplete: (String) -> Unit,
    onClick: (String) -> Unit,
    modifier: Modifier = Modifier
) {
    val isCompleted = todo.status == "completed"

    Card(
        modifier = modifier
            .fillMaxWidth()
            .padding(horizontal = 16.dp, vertical = 4.dp)
            .clickable { onClick(todo.id) },
        shape = RoundedCornerShape(12.dp),
        colors = CardDefaults.cardColors(
            containerColor = MaterialTheme.colorScheme.surface
        ),
        elevation = CardDefaults.cardElevation(defaultElevation = 2.dp)
    ) {
        Row(
            modifier = Modifier
                .fillMaxWidth()
                .padding(12.dp),
            verticalAlignment = Alignment.CenterVertically
        ) {
            // Completion checkbox
            IconButton(
                onClick = { onToggleComplete(todo.id) },
                modifier = Modifier.size(40.dp)
            ) {
                Icon(
                    imageVector = if (isCompleted) Icons.Filled.CheckCircle else Icons.Outlined.Circle,
                    contentDescription = if (isCompleted) "Mark incomplete" else "Mark complete",
                    tint = if (isCompleted) {
                        MaterialTheme.colorScheme.secondary
                    } else {
                        MaterialTheme.colorScheme.onSurfaceVariant
                    },
                    modifier = Modifier.size(28.dp)
                )
            }

            Spacer(modifier = Modifier.width(8.dp))

            Column(modifier = Modifier.weight(1f)) {
                // Title
                Text(
                    text = todo.title,
                    style = MaterialTheme.typography.bodyLarge,
                    color = if (isCompleted) {
                        MaterialTheme.colorScheme.onSurfaceVariant
                    } else {
                        MaterialTheme.colorScheme.onSurface
                    },
                    textDecoration = if (isCompleted) TextDecoration.LineThrough else null,
                    maxLines = 2,
                    overflow = TextOverflow.Ellipsis
                )

                // Due date and priority row
                Row(
                    modifier = Modifier.padding(top = 4.dp),
                    verticalAlignment = Alignment.CenterVertically
                ) {
                    // Priority badge
                    PriorityBadge(priority = todo.priority)

                    // Due date
                    todo.dueDate?.let { dateStr ->
                        Spacer(modifier = Modifier.width(8.dp))
                        DueDateChip(dateStr = dateStr)
                    }
                }
            }
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
            .size(24.dp)
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
    val displayText = try {
        val date = ZonedDateTime.parse(dateStr).toLocalDate()
        val today = LocalDate.now()
        when {
            date == today -> "Today"
            date == today.plusDays(1) -> "Tomorrow"
            date < today -> "Overdue"
            else -> date.format(DateTimeFormatter.ofPattern("MMM d"))
        }
    } catch (e: Exception) {
        dateStr.take(10)
    }

    val isOverdue = try {
        val date = ZonedDateTime.parse(dateStr).toLocalDate()
        date < LocalDate.now()
    } catch (e: Exception) {
        false
    }

    Surface(
        modifier = modifier,
        shape = RoundedCornerShape(4.dp),
        color = if (isOverdue) {
            MaterialTheme.colorScheme.errorContainer
        } else {
            MaterialTheme.colorScheme.surfaceVariant
        }
    ) {
        Text(
            text = displayText,
            style = MaterialTheme.typography.labelSmall,
            color = if (isOverdue) {
                MaterialTheme.colorScheme.onErrorContainer
            } else {
                MaterialTheme.colorScheme.onSurfaceVariant
            },
            modifier = Modifier.padding(horizontal = 6.dp, vertical = 2.dp)
        )
    }
}
