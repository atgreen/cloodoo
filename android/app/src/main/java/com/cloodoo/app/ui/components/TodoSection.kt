package com.cloodoo.app.ui.components

import androidx.compose.animation.*
import androidx.compose.animation.core.animateFloatAsState
import androidx.compose.foundation.background
import androidx.compose.foundation.clickable
import androidx.compose.foundation.layout.*
import androidx.compose.foundation.shape.RoundedCornerShape
import androidx.compose.material.icons.Icons
import androidx.compose.material.icons.filled.KeyboardArrowDown
import androidx.compose.material3.*
import androidx.compose.runtime.*
import androidx.compose.ui.Alignment
import androidx.compose.ui.Modifier
import androidx.compose.ui.draw.clip
import androidx.compose.ui.draw.rotate
import androidx.compose.ui.graphics.Color
import androidx.compose.ui.text.font.FontWeight
import androidx.compose.ui.unit.dp
import com.cloodoo.app.data.local.TodoEntity
import java.time.LocalDate
import java.time.OffsetDateTime
import java.time.ZonedDateTime
import java.time.temporal.ChronoUnit

/**
 * Date-based grouping categories for TODOs
 */
enum class DateGroup(val displayName: String, val order: Int) {
    OVERDUE("Overdue", 0),
    TODAY("Today", 1),
    TOMORROW("Tomorrow", 2),
    THIS_WEEK("This Week", 3),
    NEXT_WEEK("Next Week", 4),
    NEXT_30_DAYS("Next 30 Days", 5),
    NEXT_60_DAYS("Next 60 Days", 6),
    NEXT_90_DAYS("Next 90 Days", 7),
    LATER("Later", 8),
    NO_DATE("No Date", 9),
    COMPLETED("Completed", 10);

    companion object {
        /**
         * Parse an ISO 8601 date string to LocalDate.
         * Handles both offset format (-05:00) and zone format ([America/New_York]).
         */
        private fun parseToLocalDate(dateStr: String): LocalDate? {
            return try {
                // Try OffsetDateTime first (handles -05:00 format)
                OffsetDateTime.parse(dateStr).toLocalDate()
            } catch (e: Exception) {
                try {
                    // Fall back to ZonedDateTime (handles [Zone] format)
                    ZonedDateTime.parse(dateStr).toLocalDate()
                } catch (e2: Exception) {
                    null
                }
            }
        }

        fun fromTodo(todo: TodoEntity): DateGroup {
            // Completed/cancelled items go to COMPLETED section
            if (todo.status == "completed" || todo.status == "cancelled") return COMPLETED

            // Check overdue based on DUE DATE only (not scheduled date)
            todo.dueDate?.let { dueDateStr ->
                parseToLocalDate(dueDateStr)?.let { dueDate ->
                    val today = LocalDate.now()
                    if (dueDate < today) return OVERDUE
                }
            }

            // Use scheduled date for grouping, fall back to due date
            val dateStr = todo.scheduledDate ?: todo.dueDate
            if (dateStr == null) return NO_DATE

            val date = parseToLocalDate(dateStr) ?: return NO_DATE
            val today = LocalDate.now()
            val endOfWeek = today.plusDays(7 - today.dayOfWeek.value.toLong())
            val endOfNextWeek = endOfWeek.plusWeeks(1)
            val daysUntil = ChronoUnit.DAYS.between(today, date)

            return when {
                date < today -> TODAY  // Past scheduled dates show as TODAY
                date == today -> TODAY
                date == today.plusDays(1) -> TOMORROW
                date <= endOfWeek -> THIS_WEEK
                date <= endOfNextWeek -> NEXT_WEEK
                daysUntil <= 30 -> NEXT_30_DAYS
                daysUntil <= 60 -> NEXT_60_DAYS
                daysUntil <= 90 -> NEXT_90_DAYS
                else -> LATER
            }
        }
    }
}

/**
 * Data class representing a group of TODOs with metadata
 */
data class TodoGroupData(
    val group: DateGroup,
    val todos: List<TodoEntity>,
    val isExpanded: Boolean = true
)

/**
 * Groups TODOs by date category
 */
fun groupTodosByDate(todos: List<TodoEntity>): List<TodoGroupData> {
    return todos
        .filter { it.status != "deleted" }  // Exclude deleted items
        .groupBy { DateGroup.fromTodo(it) }
        .map { (group, items) ->
            TodoGroupData(
                group = group,
                todos = items.sortedWith(
                    compareBy(
                        { it.priority != "high" },
                        { it.priority != "medium" },
                        { it.scheduledDate ?: it.dueDate ?: "" }
                    )
                ),
                // All groups start collapsed
                isExpanded = false
            )
        }
        .sortedBy { it.group.order }
}

/**
 * Collapsible section header for a group of TODOs
 */
@Composable
fun SectionHeader(
    group: DateGroup,
    count: Int,
    isExpanded: Boolean,
    onToggle: () -> Unit,
    modifier: Modifier = Modifier
) {
    val rotationAngle by animateFloatAsState(
        targetValue = if (isExpanded) 0f else -90f,
        label = "arrow_rotation"
    )

    val (accentColor, backgroundColor) = when (group) {
        DateGroup.OVERDUE -> Pair(
            MaterialTheme.colorScheme.error,
            MaterialTheme.colorScheme.errorContainer.copy(alpha = 0.3f)
        )
        DateGroup.TODAY -> Pair(
            MaterialTheme.colorScheme.primary,
            MaterialTheme.colorScheme.primaryContainer.copy(alpha = 0.3f)
        )
        DateGroup.COMPLETED -> Pair(
            MaterialTheme.colorScheme.outline,
            MaterialTheme.colorScheme.surfaceVariant.copy(alpha = 0.5f)
        )
        else -> Pair(
            MaterialTheme.colorScheme.onSurfaceVariant,
            Color.Transparent
        )
    }

    // Outer surface with solid background to prevent items showing through
    Surface(
        modifier = modifier.fillMaxWidth(),
        color = MaterialTheme.colorScheme.background
    ) {
        Surface(
            modifier = Modifier
                .fillMaxWidth()
                .padding(horizontal = 8.dp, vertical = 4.dp)
            .clip(RoundedCornerShape(8.dp))
            .clickable { onToggle() },
        color = backgroundColor
    ) {
        Row(
            modifier = Modifier
                .fillMaxWidth()
                .padding(horizontal = 12.dp, vertical = 10.dp),
            verticalAlignment = Alignment.CenterVertically
        ) {
            // Expand/collapse arrow
            Icon(
                imageVector = Icons.Default.KeyboardArrowDown,
                contentDescription = if (isExpanded) "Collapse" else "Expand",
                modifier = Modifier
                    .size(24.dp)
                    .rotate(rotationAngle),
                tint = accentColor
            )

            Spacer(modifier = Modifier.width(8.dp))

            // Section title
            Text(
                text = group.displayName,
                style = MaterialTheme.typography.titleSmall,
                fontWeight = FontWeight.SemiBold,
                color = accentColor
            )

            Spacer(modifier = Modifier.width(8.dp))

            // Item count badge
            Surface(
                shape = RoundedCornerShape(12.dp),
                color = accentColor.copy(alpha = 0.15f)
            ) {
                Text(
                    text = count.toString(),
                    style = MaterialTheme.typography.labelMedium,
                    color = accentColor,
                    modifier = Modifier.padding(horizontal = 8.dp, vertical = 2.dp)
                )
            }

            Spacer(modifier = Modifier.weight(1f))

            // Optional: Show checkmark for completed section
            if (group == DateGroup.COMPLETED) {
                Text(
                    text = "✓",
                    style = MaterialTheme.typography.titleSmall,
                    color = accentColor
                )
            }
        }
        }
    }
}

/**
 * A collapsible section containing a header and list of TODOs
 */
@Composable
fun CollapsibleTodoSection(
    groupData: TodoGroupData,
    onToggleExpanded: () -> Unit,
    onToggleComplete: (String) -> Unit,
    onTodoClick: (String) -> Unit,
    onTodoCancel: (String) -> Unit,
    modifier: Modifier = Modifier
) {
    Column(modifier = modifier) {
        SectionHeader(
            group = groupData.group,
            count = groupData.todos.size,
            isExpanded = groupData.isExpanded,
            onToggle = onToggleExpanded
        )

        AnimatedVisibility(
            visible = groupData.isExpanded,
            enter = expandVertically() + fadeIn(),
            exit = shrinkVertically() + fadeOut()
        ) {
            Column {
                groupData.todos.forEach { todo ->
                    key(todo.rowId) {
                        SwipeableTodoItem(
                            todo = todo,
                            onToggleComplete = onToggleComplete,
                            onClick = onTodoClick,
                            onCancel = onTodoCancel
                        )
                    }
                }
            }
        }
    }
}
