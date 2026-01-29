package com.cloodoo.app.ui.screens

import androidx.compose.foundation.horizontalScroll
import androidx.compose.foundation.layout.*
import androidx.compose.foundation.rememberScrollState
import androidx.compose.foundation.verticalScroll
import androidx.compose.material.icons.Icons
import androidx.compose.material.icons.filled.Clear
import androidx.compose.material.icons.filled.Close
import androidx.compose.material3.*
import androidx.compose.runtime.*
import androidx.compose.ui.Alignment
import androidx.compose.ui.Modifier
import androidx.compose.ui.focus.FocusRequester
import androidx.compose.ui.focus.focusRequester
import androidx.compose.ui.unit.dp
import androidx.compose.runtime.mutableIntStateOf
import com.cloodoo.app.ui.util.formatDate
import com.cloodoo.app.ui.util.millisToIsoDate
import com.cloodoo.app.ui.util.tagsToStorageString

@OptIn(ExperimentalMaterial3Api::class)
@Composable
fun AddTaskScreen(
    viewModel: TodoListViewModel,
    onNavigateBack: () -> Unit
) {
    var title by remember { mutableStateOf("") }
    var description by remember { mutableStateOf("") }
    var priority by remember { mutableStateOf("medium") }
    var dueDate by remember { mutableStateOf<String?>(null) }
    var scheduledDate by remember { mutableStateOf<String?>(null) }
    var tags by remember { mutableStateOf<List<String>>(emptyList()) }
    var newTagText by remember { mutableStateOf("") }
    var showDueDatePicker by remember { mutableStateOf(false) }
    var showScheduledDatePicker by remember { mutableStateOf(false) }
    var repeatUnit by remember { mutableStateOf<String?>(null) }
    var repeatInterval by remember { mutableIntStateOf(1) }

    val focusRequester = remember { FocusRequester() }
    LaunchedEffect(Unit) { focusRequester.requestFocus() }

    Scaffold(
        topBar = {
            TopAppBar(
                title = { Text("New Task") },
                navigationIcon = {
                    IconButton(onClick = onNavigateBack) {
                        Icon(Icons.Default.Close, contentDescription = "Close")
                    }
                },
                actions = {
                    TextButton(
                        onClick = {
                            viewModel.createTodo(
                                title = title,
                                description = description.ifBlank { null },
                                priority = priority,
                                dueDate = dueDate,
                                scheduledDate = scheduledDate,
                                tags = if (tags.isNotEmpty()) tagsToStorageString(tags) else null,
                                repeatInterval = if (repeatUnit != null) repeatInterval else null,
                                repeatUnit = repeatUnit
                            )
                            onNavigateBack()
                        },
                        enabled = title.isNotBlank()
                    ) {
                        Text("Save")
                    }
                }
            )
        }
    ) { padding ->
        Column(
            modifier = Modifier
                .fillMaxSize()
                .padding(padding)
                .verticalScroll(rememberScrollState())
                .padding(16.dp),
            verticalArrangement = Arrangement.spacedBy(16.dp)
        ) {
            // Title
            OutlinedTextField(
                value = title,
                onValueChange = { title = it },
                label = { Text("Title") },
                singleLine = true,
                modifier = Modifier
                    .fillMaxWidth()
                    .focusRequester(focusRequester)
            )

            // Description
            OutlinedTextField(
                value = description,
                onValueChange = { description = it },
                label = { Text("Description") },
                modifier = Modifier
                    .fillMaxWidth()
                    .heightIn(min = 100.dp),
                maxLines = 6
            )

            // Priority
            Text("Priority", style = MaterialTheme.typography.labelLarge)
            Row(
                horizontalArrangement = Arrangement.spacedBy(8.dp)
            ) {
                listOf("high" to "High", "medium" to "Medium", "low" to "Low").forEach { (value, label) ->
                    FilterChip(
                        selected = priority == value,
                        onClick = { priority = value },
                        label = { Text(label) }
                    )
                }
            }

            // Due Date
            Row(verticalAlignment = Alignment.CenterVertically) {
                OutlinedButton(onClick = { showDueDatePicker = true }) {
                    Text(dueDate?.let { "Due: ${formatDate(it)}" } ?: "Set Due Date")
                }
                if (dueDate != null) {
                    IconButton(onClick = { dueDate = null }) {
                        Icon(Icons.Default.Clear, contentDescription = "Clear due date")
                    }
                }
            }

            // Scheduled Date
            Row(verticalAlignment = Alignment.CenterVertically) {
                OutlinedButton(onClick = { showScheduledDatePicker = true }) {
                    Text(scheduledDate?.let { "Scheduled: ${formatDate(it)}" } ?: "Set Scheduled Date")
                }
                if (scheduledDate != null) {
                    IconButton(onClick = { scheduledDate = null }) {
                        Icon(Icons.Default.Clear, contentDescription = "Clear scheduled date")
                    }
                }
            }

            // Tags
            Text("Tags", style = MaterialTheme.typography.labelLarge)
            if (tags.isNotEmpty()) {
                Row(
                    horizontalArrangement = Arrangement.spacedBy(8.dp),
                    modifier = Modifier.horizontalScroll(rememberScrollState())
                ) {
                    tags.forEach { tag ->
                        InputChip(
                            selected = false,
                            onClick = { },
                            label = { Text(tag) },
                            trailingIcon = {
                                IconButton(
                                    onClick = { tags = tags - tag },
                                    modifier = Modifier.size(18.dp)
                                ) {
                                    Icon(
                                        Icons.Default.Clear,
                                        contentDescription = "Remove tag",
                                        modifier = Modifier.size(14.dp)
                                    )
                                }
                            }
                        )
                    }
                }
            }
            Row(
                verticalAlignment = Alignment.CenterVertically,
                horizontalArrangement = Arrangement.spacedBy(8.dp)
            ) {
                OutlinedTextField(
                    value = newTagText,
                    onValueChange = { newTagText = it },
                    placeholder = { Text("Add tag") },
                    singleLine = true,
                    modifier = Modifier.weight(1f),
                    textStyle = MaterialTheme.typography.bodyMedium
                )
                TextButton(
                    onClick = {
                        val trimmed = newTagText.trim()
                        if (trimmed.isNotEmpty() && trimmed !in tags) {
                            tags = tags + trimmed
                            newTagText = ""
                        }
                    },
                    enabled = newTagText.isNotBlank()
                ) {
                    Text("Add")
                }
            }

            // Recurrence
            Text("Recurrence", style = MaterialTheme.typography.labelLarge)
            Row(
                horizontalArrangement = Arrangement.spacedBy(8.dp)
            ) {
                listOf(null to "None", "day" to "Daily", "week" to "Weekly", "month" to "Monthly").forEach { (value, label) ->
                    FilterChip(
                        selected = repeatUnit == value,
                        onClick = { repeatUnit = value; repeatInterval = 1 },
                        label = { Text(label) }
                    )
                }
            }
            if (repeatUnit != null) {
                Row(
                    verticalAlignment = Alignment.CenterVertically,
                    horizontalArrangement = Arrangement.spacedBy(8.dp)
                ) {
                    Text("Every", style = MaterialTheme.typography.bodyMedium)
                    OutlinedTextField(
                        value = repeatInterval.toString(),
                        onValueChange = { text ->
                            text.toIntOrNull()?.let { if (it > 0) repeatInterval = it }
                        },
                        singleLine = true,
                        modifier = Modifier.width(64.dp),
                        textStyle = MaterialTheme.typography.bodyMedium
                    )
                    Text(
                        when (repeatUnit) {
                            "day" -> if (repeatInterval == 1) "day" else "days"
                            "week" -> if (repeatInterval == 1) "week" else "weeks"
                            "month" -> if (repeatInterval == 1) "month" else "months"
                            else -> ""
                        },
                        style = MaterialTheme.typography.bodyMedium
                    )
                }
            }
        }
    }

    // Due date picker
    if (showDueDatePicker) {
        val datePickerState = rememberDatePickerState()
        DatePickerDialog(
            onDismissRequest = { showDueDatePicker = false },
            confirmButton = {
                TextButton(onClick = {
                    datePickerState.selectedDateMillis?.let { millis ->
                        dueDate = millisToIsoDate(millis)
                    }
                    showDueDatePicker = false
                }) { Text("OK") }
            },
            dismissButton = {
                TextButton(onClick = { showDueDatePicker = false }) { Text("Cancel") }
            }
        ) {
            DatePicker(state = datePickerState)
        }
    }

    // Scheduled date picker
    if (showScheduledDatePicker) {
        val datePickerState = rememberDatePickerState()
        DatePickerDialog(
            onDismissRequest = { showScheduledDatePicker = false },
            confirmButton = {
                TextButton(onClick = {
                    datePickerState.selectedDateMillis?.let { millis ->
                        scheduledDate = millisToIsoDate(millis)
                    }
                    showScheduledDatePicker = false
                }) { Text("OK") }
            },
            dismissButton = {
                TextButton(onClick = { showScheduledDatePicker = false }) { Text("Cancel") }
            }
        ) {
            DatePicker(state = datePickerState)
        }
    }
}
