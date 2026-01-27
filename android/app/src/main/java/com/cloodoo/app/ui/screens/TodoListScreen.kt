package com.cloodoo.app.ui.screens

import android.app.Application
import androidx.compose.animation.AnimatedVisibility
import androidx.compose.foundation.clickable
import androidx.compose.foundation.horizontalScroll
import androidx.compose.foundation.layout.*
import androidx.compose.foundation.ExperimentalFoundationApi
import androidx.compose.foundation.lazy.LazyColumn
import androidx.compose.foundation.lazy.items
import androidx.compose.foundation.rememberScrollState
import androidx.compose.material.icons.Icons
import androidx.compose.material.icons.filled.Add
import androidx.compose.material.icons.filled.Clear
import androidx.compose.material.icons.filled.Cloud
import androidx.compose.material.icons.filled.CloudOff
import androidx.compose.material.icons.filled.Search
import androidx.compose.material.icons.filled.Settings
import androidx.compose.material3.*
import androidx.compose.material3.rememberModalBottomSheetState
import androidx.compose.runtime.*
import androidx.compose.ui.Alignment
import androidx.compose.ui.Modifier
import androidx.compose.ui.focus.FocusRequester
import androidx.compose.ui.focus.focusRequester
import androidx.compose.ui.graphics.Color
import androidx.compose.ui.platform.LocalContext
import androidx.compose.ui.text.withStyle
import androidx.compose.ui.unit.dp
import androidx.lifecycle.viewmodel.compose.viewModel
import com.cloodoo.app.data.local.TodoEntity
import com.cloodoo.app.data.remote.ConnectionState
import com.cloodoo.app.data.security.CertificateManager
import com.cloodoo.app.ui.components.CollapsibleTodoSection
import nl.dionsegijn.konfetti.compose.KonfettiView
import nl.dionsegijn.konfetti.core.Party
import nl.dionsegijn.konfetti.core.Position
import nl.dionsegijn.konfetti.core.emitter.Emitter
import java.util.concurrent.TimeUnit

@OptIn(ExperimentalMaterial3Api::class, ExperimentalFoundationApi::class)
@Composable
fun TodoListScreen(
    certificateManager: CertificateManager
) {
    val context = LocalContext.current
    val application = context.applicationContext as Application

    val viewModel: TodoListViewModel = viewModel(
        factory = TodoListViewModel.Factory(application, certificateManager)
    )

    val uiState by viewModel.uiState.collectAsState()
    val connectionState by viewModel.connectionState.collectAsState()

    var showAddDialog by remember { mutableStateOf(false) }
    var showSettingsDialog by remember { mutableStateOf(false) }
    var searchQuery by remember { mutableStateOf("") }
    var isSearchVisible by remember { mutableStateOf(false) }
    var selectedTag by remember { mutableStateOf<String?>(null) }
    var selectedTodo by remember { mutableStateOf<TodoEntity?>(null) }
    var todoToConfirmComplete by remember { mutableStateOf<TodoEntity?>(null) }

    // Get all unique tags from todos - handle both JSON arrays and comma-separated
    val allTags = remember(uiState.todos) {
        uiState.todos
            .flatMap { todo ->
                todo.tags?.let { tagsStr ->
                    try {
                        if (tagsStr.startsWith("[")) {
                            com.google.gson.Gson().fromJson<List<String>>(
                                tagsStr,
                                object : com.google.gson.reflect.TypeToken<List<String>>() {}.type
                            ) ?: emptyList()
                        } else {
                            tagsStr.split(",").map { it.trim() }.filter { it.isNotEmpty() }
                        }
                    } catch (e: Exception) {
                        tagsStr.split(",").map { it.trim() }.filter { it.isNotEmpty() }
                    }
                } ?: emptyList()
            }
            .distinct()
            .sorted()
    }

    // Filter todos based on search and tag
    val filteredGroupedTodos = remember(uiState.groupedTodos, searchQuery, selectedTag) {
        val currentTag = selectedTag
        if (searchQuery.isBlank() && currentTag == null) {
            uiState.groupedTodos
        } else {
            uiState.groupedTodos.mapNotNull { group ->
                val filteredTodos = group.todos.filter { todo ->
                    val matchesSearch = searchQuery.isBlank() ||
                        todo.title.contains(searchQuery, ignoreCase = true) ||
                        (todo.description?.contains(searchQuery, ignoreCase = true) == true)
                    val matchesTag = currentTag == null ||
                        (todo.tags?.contains(currentTag) == true)
                    matchesSearch && matchesTag
                }
                if (filteredTodos.isNotEmpty()) {
                    group.copy(todos = filteredTodos)
                } else null
            }
        }
    }

    // Show snackbar for sync results
    val snackbarHostState = remember { SnackbarHostState() }
    LaunchedEffect(uiState.lastSyncResult, uiState.syncError) {
        uiState.lastSyncResult?.let {
            snackbarHostState.showSnackbar(it)
            viewModel.clearSyncMessage()
        }
        uiState.syncError?.let {
            snackbarHostState.showSnackbar("Error: $it")
            viewModel.clearSyncMessage()
        }
    }

    Scaffold(
        topBar = {
            TopAppBar(
                title = { Text("Cloodoo") },
                colors = TopAppBarDefaults.topAppBarColors(
                    containerColor = MaterialTheme.colorScheme.primaryContainer,
                    titleContentColor = MaterialTheme.colorScheme.onPrimaryContainer
                ),
                actions = {
                    // Search toggle button
                    IconButton(onClick = { isSearchVisible = !isSearchVisible }) {
                        Icon(
                            Icons.Default.Search,
                            contentDescription = "Search"
                        )
                    }
                    // Settings button
                    IconButton(onClick = { showSettingsDialog = true }) {
                        Icon(
                            Icons.Default.Settings,
                            contentDescription = "Settings"
                        )
                    }
                    // Connection status indicator - always show something
                    IconButton(onClick = { viewModel.connect() }) {
                        when (connectionState) {
                            ConnectionState.CONNECTED -> {
                                Icon(
                                    Icons.Default.Cloud,
                                    contentDescription = "Connected",
                                    tint = MaterialTheme.colorScheme.primary
                                )
                            }
                            ConnectionState.CONNECTING -> {
                                CircularProgressIndicator(
                                    modifier = Modifier.size(24.dp),
                                    strokeWidth = 2.dp
                                )
                            }
                            ConnectionState.ERROR -> {
                                Icon(
                                    Icons.Default.CloudOff,
                                    contentDescription = "Error - tap to reconnect",
                                    tint = MaterialTheme.colorScheme.error
                                )
                            }
                            ConnectionState.DISCONNECTED -> {
                                Icon(
                                    Icons.Default.CloudOff,
                                    contentDescription = "Disconnected - tap to reconnect",
                                    tint = MaterialTheme.colorScheme.onSurfaceVariant
                                )
                            }
                        }
                    }
                }
            )
        },
        floatingActionButton = {
            FloatingActionButton(
                onClick = { showAddDialog = true },
                containerColor = MaterialTheme.colorScheme.primary
            ) {
                Icon(Icons.Default.Add, contentDescription = "Add TODO")
            }
        },
        snackbarHost = { SnackbarHost(snackbarHostState) }
    ) { padding ->
        Column(
            modifier = Modifier
                .fillMaxSize()
                .padding(padding)
        ) {
            // Search bar
            AnimatedVisibility(visible = isSearchVisible) {
                val focusRequester = remember { FocusRequester() }
                LaunchedEffect(Unit) {
                    focusRequester.requestFocus()
                }
                OutlinedTextField(
                    value = searchQuery,
                    onValueChange = { searchQuery = it },
                    modifier = Modifier
                        .fillMaxWidth()
                        .padding(horizontal = 12.dp, vertical = 8.dp)
                        .focusRequester(focusRequester),
                    placeholder = { Text("Search todos...") },
                    leadingIcon = { Icon(Icons.Default.Search, contentDescription = "Search") },
                    trailingIcon = {
                        IconButton(onClick = {
                            searchQuery = ""
                            isSearchVisible = false
                        }) {
                            Icon(Icons.Default.Clear, contentDescription = "Close search")
                        }
                    },
                    singleLine = true
                )
            }

            // Tag filter chips
            if (allTags.isNotEmpty()) {
                Row(
                    modifier = Modifier
                        .horizontalScroll(rememberScrollState())
                        .padding(horizontal = 12.dp, vertical = 4.dp),
                    horizontalArrangement = Arrangement.spacedBy(8.dp)
                ) {
                    FilterChip(
                        selected = selectedTag == null,
                        onClick = { selectedTag = null },
                        label = { Text("All") }
                    )
                    allTags.forEach { tag ->
                        FilterChip(
                            selected = selectedTag == tag,
                            onClick = { selectedTag = if (selectedTag == tag) null else tag },
                            label = { Text(tag) }
                        )
                    }
                }
            }

            // Content
            Box(modifier = Modifier.weight(1f)) {
                if (uiState.isLoading) {
                    CircularProgressIndicator(
                        modifier = Modifier.align(Alignment.Center)
                    )
                } else if (uiState.todos.isEmpty()) {
                    Column(
                        modifier = Modifier.align(Alignment.Center),
                        horizontalAlignment = Alignment.CenterHorizontally
                    ) {
                        Text(
                            text = "No TODOs yet",
                            style = MaterialTheme.typography.titleMedium,
                            color = MaterialTheme.colorScheme.onSurfaceVariant
                        )
                        Spacer(modifier = Modifier.height(8.dp))
                        Text(
                            text = "Tap + to add one",
                            style = MaterialTheme.typography.bodyMedium,
                            color = MaterialTheme.colorScheme.onSurfaceVariant
                        )
                    }
                } else if (filteredGroupedTodos.isEmpty()) {
                    Column(
                        modifier = Modifier.align(Alignment.Center),
                        horizontalAlignment = Alignment.CenterHorizontally
                    ) {
                        Text(
                            text = "No matching TODOs",
                            style = MaterialTheme.typography.titleMedium,
                            color = MaterialTheme.colorScheme.onSurfaceVariant
                        )
                    }
                } else {
                    LazyColumn(
                        modifier = Modifier.fillMaxSize(),
                        contentPadding = PaddingValues(vertical = 8.dp)
                    ) {
                        filteredGroupedTodos.forEach { groupData ->
                            stickyHeader(key = "header_${groupData.group.name}") {
                                com.cloodoo.app.ui.components.SectionHeader(
                                    group = groupData.group,
                                    count = groupData.todos.size,
                                    isExpanded = groupData.isExpanded,
                                    onToggle = { viewModel.toggleSectionExpanded(groupData.group) }
                                )
                            }
                            if (groupData.isExpanded) {
                                items(
                                    items = groupData.todos,
                                    key = { it.rowId }
                                ) { todo ->
                                    com.cloodoo.app.ui.components.SwipeableTodoItem(
                                        todo = todo,
                                        onToggleComplete = { todoId ->
                                            // Show confirmation dialog instead of directly toggling
                                            todoToConfirmComplete = uiState.todos.find { t -> t.id == todoId }
                                        },
                                        onClick = { selectedTodo = uiState.todos.find { t -> t.id == it } },
                                        onDelete = { viewModel.deleteTodo(it) }
                                    )
                                }
                            }
                        }
                    }
                }
            }
        }
    }

    // Confetti celebration when completing a task
    if (uiState.showConfetti) {
        KonfettiView(
            modifier = Modifier.fillMaxSize(),
            parties = listOf(
                Party(
                    speed = 0f,
                    maxSpeed = 30f,
                    damping = 0.9f,
                    spread = 360,
                    colors = listOf(
                        Color(0xFFE91E63).hashCode(),  // Pink
                        Color(0xFF9C27B0).hashCode(),  // Purple
                        Color(0xFF2196F3).hashCode(),  // Blue
                        Color(0xFF4CAF50).hashCode(),  // Green
                        Color(0xFFFFEB3B).hashCode(),  // Yellow
                        Color(0xFFFF9800).hashCode()   // Orange
                    ),
                    position = Position.Relative(0.5, 0.3),
                    emitter = Emitter(duration = 100, TimeUnit.MILLISECONDS).max(50)
                )
            )
        )
        // Clear confetti after animation
        LaunchedEffect(Unit) {
            kotlinx.coroutines.delay(2000)
            viewModel.clearConfetti()
        }
    }

    // Keep selectedTodo fresh from uiState
    LaunchedEffect(uiState.todos) {
        selectedTodo?.let { sel ->
            selectedTodo = uiState.todos.find { it.id == sel.id }
        }
    }

    // Detail Bottom Sheet
    if (selectedTodo != null) {
        TodoDetailSheet(
            todo = selectedTodo!!,
            onDismiss = { selectedTodo = null },
            onUpdate = { todoId, dueDate, scheduledDate, tags ->
                viewModel.updateTodo(todoId, dueDate, scheduledDate, tags)
            }
        )
    }

    // Add TODO Dialog
    if (showAddDialog) {
        AddTodoDialog(
            onDismiss = { showAddDialog = false },
            onConfirm = { title, priority ->
                viewModel.createTodo(title, priority)
                showAddDialog = false
            }
        )
    }

    // Settings Dialog
    if (showSettingsDialog) {
        ServerSettingsDialog(
            certificateManager = certificateManager,
            onDismiss = { showSettingsDialog = false },
            onSave = { address, port ->
                certificateManager.updateServerConfig(address, port)
                viewModel.disconnect()
                viewModel.connect()
                showSettingsDialog = false
            }
        )
    }

    // Completion Confirmation Dialog
    todoToConfirmComplete?.let { todo ->
        val isCurrentlyCompleted = todo.status == "completed"
        AlertDialog(
            onDismissRequest = { todoToConfirmComplete = null },
            title = {
                Text(if (isCurrentlyCompleted) "Reopen Task?" else "Complete Task?")
            },
            text = {
                Text(
                    if (isCurrentlyCompleted)
                        "Mark \"${todo.title}\" as not completed?"
                    else
                        "Mark \"${todo.title}\" as completed?"
                )
            },
            confirmButton = {
                TextButton(
                    onClick = {
                        viewModel.toggleComplete(todo.id)
                        todoToConfirmComplete = null
                    }
                ) {
                    Text(if (isCurrentlyCompleted) "Reopen" else "Complete")
                }
            },
            dismissButton = {
                TextButton(onClick = { todoToConfirmComplete = null }) {
                    Text("Cancel")
                }
            }
        )
    }
}

@OptIn(ExperimentalMaterial3Api::class)
@Composable
fun AddTodoDialog(
    onDismiss: () -> Unit,
    onConfirm: (String, String) -> Unit
) {
    var title by remember { mutableStateOf("") }
    var priority by remember { mutableStateOf("medium") }

    AlertDialog(
        onDismissRequest = onDismiss,
        title = { Text("New TODO") },
        text = {
            Column {
                OutlinedTextField(
                    value = title,
                    onValueChange = { title = it },
                    label = { Text("Title") },
                    singleLine = true,
                    modifier = Modifier.fillMaxWidth()
                )
                Spacer(modifier = Modifier.height(16.dp))
                Text("Priority", style = MaterialTheme.typography.labelMedium)
                Row(
                    modifier = Modifier.fillMaxWidth(),
                    horizontalArrangement = Arrangement.SpaceEvenly
                ) {
                    listOf("high" to "High", "medium" to "Medium", "low" to "Low").forEach { (value, label) ->
                        FilterChip(
                            selected = priority == value,
                            onClick = { priority = value },
                            label = { Text(label) }
                        )
                    }
                }
            }
        },
        confirmButton = {
            TextButton(
                onClick = { onConfirm(title, priority) },
                enabled = title.isNotBlank()
            ) {
                Text("Add")
            }
        },
        dismissButton = {
            TextButton(onClick = onDismiss) {
                Text("Cancel")
            }
        }
    )
}

@OptIn(ExperimentalMaterial3Api::class)
@Composable
fun ServerSettingsDialog(
    certificateManager: CertificateManager,
    onDismiss: () -> Unit,
    onSave: (String, Int) -> Unit
) {
    var serverAddress by remember { mutableStateOf(certificateManager.getServerAddress() ?: "") }
    var serverPort by remember { mutableStateOf(certificateManager.getServerPort().toString()) }

    AlertDialog(
        onDismissRequest = onDismiss,
        title = { Text("Server Settings") },
        text = {
            Column {
                OutlinedTextField(
                    value = serverAddress,
                    onValueChange = { serverAddress = it },
                    label = { Text("Server Address") },
                    singleLine = true,
                    modifier = Modifier.fillMaxWidth()
                )
                Spacer(modifier = Modifier.height(16.dp))
                OutlinedTextField(
                    value = serverPort,
                    onValueChange = { serverPort = it.filter { c -> c.isDigit() } },
                    label = { Text("Port") },
                    singleLine = true,
                    modifier = Modifier.fillMaxWidth()
                )
            }
        },
        confirmButton = {
            TextButton(
                onClick = {
                    val port = serverPort.toIntOrNull() ?: 50051
                    onSave(serverAddress, port)
                },
                enabled = serverAddress.isNotBlank() && serverPort.isNotBlank()
            ) {
                Text("Save")
            }
        },
        dismissButton = {
            TextButton(onClick = onDismiss) {
                Text("Cancel")
            }
        }
    )
}

@OptIn(ExperimentalMaterial3Api::class)
@Composable
fun TodoDetailSheet(
    todo: TodoEntity,
    onDismiss: () -> Unit,
    onUpdate: (todoId: String, dueDate: String?, scheduledDate: String?, tags: String?) -> Unit
) {
    val context = LocalContext.current
    val sheetState = rememberModalBottomSheetState(skipPartiallyExpanded = true)

    // Parse tags - handle both JSON arrays and comma-separated strings
    val tags = remember(todo.tags) {
        todo.tags?.let { tagsStr ->
            try {
                if (tagsStr.startsWith("[")) {
                    com.google.gson.Gson().fromJson<List<String>>(
                        tagsStr,
                        object : com.google.gson.reflect.TypeToken<List<String>>() {}.type
                    ) ?: emptyList()
                } else {
                    tagsStr.split(",").map { it.trim() }.filter { it.isNotEmpty() }
                }
            } catch (e: Exception) {
                tagsStr.split(",").map { it.trim() }.filter { it.isNotEmpty() }
            }
        } ?: emptyList()
    }

    // Editable state for tags
    var editableTags by remember(todo.tags) { mutableStateOf(tags) }
    var newTagText by remember { mutableStateOf("") }

    // Date picker state
    var showDueDatePicker by remember { mutableStateOf(false) }
    var showScheduledDatePicker by remember { mutableStateOf(false) }

    ModalBottomSheet(
        onDismissRequest = onDismiss,
        sheetState = sheetState
    ) {
        Column(
            modifier = Modifier
                .fillMaxWidth()
                .padding(horizontal = 20.dp, vertical = 16.dp)
                .padding(bottom = 32.dp)
        ) {
            // Title
            Text(
                text = todo.title,
                style = MaterialTheme.typography.headlineSmall,
                color = MaterialTheme.colorScheme.onSurface
            )

            Spacer(modifier = Modifier.height(16.dp))

            // Status and Priority row
            Row(
                horizontalArrangement = Arrangement.spacedBy(8.dp)
            ) {
                AssistChip(
                    onClick = { },
                    label = { Text(todo.status.replaceFirstChar { it.uppercase() }) },
                    colors = AssistChipDefaults.assistChipColors(
                        containerColor = when (todo.status) {
                            "completed" -> MaterialTheme.colorScheme.primaryContainer
                            "cancelled" -> MaterialTheme.colorScheme.errorContainer
                            else -> MaterialTheme.colorScheme.surfaceVariant
                        }
                    )
                )
                AssistChip(
                    onClick = { },
                    label = { Text(todo.priority.replaceFirstChar { it.uppercase() }) },
                    colors = AssistChipDefaults.assistChipColors(
                        containerColor = when (todo.priority) {
                            "high" -> MaterialTheme.colorScheme.errorContainer
                            "medium" -> MaterialTheme.colorScheme.tertiaryContainer
                            else -> MaterialTheme.colorScheme.surfaceVariant
                        }
                    )
                )
            }

            // Dates section (always shown, editable)
            Spacer(modifier = Modifier.height(16.dp))
            HorizontalDivider()
            Spacer(modifier = Modifier.height(16.dp))

            // Due date - clickable to edit
            Row(verticalAlignment = Alignment.CenterVertically) {
                Text(
                    text = "Due: ",
                    style = MaterialTheme.typography.labelLarge,
                    color = MaterialTheme.colorScheme.onSurfaceVariant
                )
                Text(
                    text = todo.dueDate?.let { formatDate(it) } ?: "Not set",
                    style = MaterialTheme.typography.bodyMedium,
                    color = if (todo.dueDate != null) MaterialTheme.colorScheme.onSurface
                            else MaterialTheme.colorScheme.onSurfaceVariant.copy(alpha = 0.6f),
                    modifier = Modifier.clickable { showDueDatePicker = true }
                )
                if (todo.dueDate != null) {
                    IconButton(
                        onClick = { onUpdate(todo.id, "", null, null) },
                        modifier = Modifier.size(32.dp)
                    ) {
                        Icon(
                            Icons.Default.Clear,
                            contentDescription = "Clear due date",
                            modifier = Modifier.size(16.dp)
                        )
                    }
                }
            }

            Spacer(modifier = Modifier.height(8.dp))

            // Scheduled date - clickable to edit
            Row(verticalAlignment = Alignment.CenterVertically) {
                Text(
                    text = "Scheduled: ",
                    style = MaterialTheme.typography.labelLarge,
                    color = MaterialTheme.colorScheme.onSurfaceVariant
                )
                Text(
                    text = todo.scheduledDate?.let { formatDate(it) } ?: "Not set",
                    style = MaterialTheme.typography.bodyMedium,
                    color = if (todo.scheduledDate != null) MaterialTheme.colorScheme.onSurface
                            else MaterialTheme.colorScheme.onSurfaceVariant.copy(alpha = 0.6f),
                    modifier = Modifier.clickable { showScheduledDatePicker = true }
                )
                if (todo.scheduledDate != null) {
                    IconButton(
                        onClick = { onUpdate(todo.id, null, "", null) },
                        modifier = Modifier.size(32.dp)
                    ) {
                        Icon(
                            Icons.Default.Clear,
                            contentDescription = "Clear scheduled date",
                            modifier = Modifier.size(16.dp)
                        )
                    }
                }
            }

            // Tags section (always shown, editable)
            Spacer(modifier = Modifier.height(16.dp))
            HorizontalDivider()
            Spacer(modifier = Modifier.height(16.dp))

            Text(
                text = "Tags",
                style = MaterialTheme.typography.labelLarge,
                color = MaterialTheme.colorScheme.onSurfaceVariant
            )
            Spacer(modifier = Modifier.height(8.dp))

            // Existing tags as removable chips
            if (editableTags.isNotEmpty()) {
                Row(
                    horizontalArrangement = Arrangement.spacedBy(8.dp),
                    modifier = Modifier.horizontalScroll(rememberScrollState())
                ) {
                    editableTags.forEach { tag ->
                        InputChip(
                            selected = false,
                            onClick = { },
                            label = { Text(tag) },
                            trailingIcon = {
                                IconButton(
                                    onClick = {
                                        editableTags = editableTags - tag
                                        val newTagsCsv = editableTags.joinToString(",").ifEmpty { "" }
                                        onUpdate(todo.id, null, null, newTagsCsv)
                                    },
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
                Spacer(modifier = Modifier.height(8.dp))
            }

            // Add new tag
            Row(
                verticalAlignment = Alignment.CenterVertically,
                horizontalArrangement = Arrangement.spacedBy(8.dp)
            ) {
                OutlinedTextField(
                    value = newTagText,
                    onValueChange = { newTagText = it },
                    placeholder = { Text("New tag") },
                    singleLine = true,
                    modifier = Modifier.weight(1f),
                    textStyle = MaterialTheme.typography.bodyMedium
                )
                TextButton(
                    onClick = {
                        val trimmed = newTagText.trim()
                        if (trimmed.isNotEmpty() && trimmed !in editableTags) {
                            editableTags = editableTags + trimmed
                            newTagText = ""
                            val newTagsCsv = editableTags.joinToString(",")
                            onUpdate(todo.id, null, null, newTagsCsv)
                        }
                    },
                    enabled = newTagText.isNotBlank()
                ) {
                    Text("Add")
                }
            }

            // URL section
            if (!todo.url.isNullOrBlank()) {
                Spacer(modifier = Modifier.height(16.dp))
                HorizontalDivider()
                Spacer(modifier = Modifier.height(16.dp))

                Text(
                    text = "Link",
                    style = MaterialTheme.typography.labelLarge,
                    color = MaterialTheme.colorScheme.onSurfaceVariant
                )
                Spacer(modifier = Modifier.height(8.dp))
                Text(
                    text = todo.url,
                    style = MaterialTheme.typography.bodyMedium,
                    color = MaterialTheme.colorScheme.primary,
                    maxLines = 2,
                    overflow = androidx.compose.ui.text.style.TextOverflow.Ellipsis,
                    modifier = Modifier.clickable {
                        try {
                            val intent = android.content.Intent(
                                android.content.Intent.ACTION_VIEW,
                                android.net.Uri.parse(todo.url)
                            )
                            context.startActivity(intent)
                        } catch (e: Exception) { /* ignore */ }
                    }
                )
            }

            // Description section
            if (!todo.description.isNullOrBlank()) {
                Spacer(modifier = Modifier.height(16.dp))
                HorizontalDivider()
                Spacer(modifier = Modifier.height(16.dp))

                Text(
                    text = "Description",
                    style = MaterialTheme.typography.labelLarge,
                    color = MaterialTheme.colorScheme.onSurfaceVariant
                )
                Spacer(modifier = Modifier.height(8.dp))
                ClickableTextWithLinks(
                    text = todo.description,
                    style = MaterialTheme.typography.bodyMedium,
                    color = MaterialTheme.colorScheme.onSurface
                )
            }
        }
    }

    // Due date picker dialog
    if (showDueDatePicker) {
        val datePickerState = rememberDatePickerState(
            initialSelectedDateMillis = todo.dueDate?.let { parseDateToMillis(it) }
        )
        DatePickerDialog(
            onDismissRequest = { showDueDatePicker = false },
            confirmButton = {
                TextButton(onClick = {
                    datePickerState.selectedDateMillis?.let { millis ->
                        val isoDate = millisToIsoDate(millis)
                        onUpdate(todo.id, isoDate, null, null)
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

    // Scheduled date picker dialog
    if (showScheduledDatePicker) {
        val datePickerState = rememberDatePickerState(
            initialSelectedDateMillis = todo.scheduledDate?.let { parseDateToMillis(it) }
        )
        DatePickerDialog(
            onDismissRequest = { showScheduledDatePicker = false },
            confirmButton = {
                TextButton(onClick = {
                    datePickerState.selectedDateMillis?.let { millis ->
                        val isoDate = millisToIsoDate(millis)
                        onUpdate(todo.id, null, isoDate, null)
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

@Composable
fun ClickableTextWithLinks(
    text: String,
    style: androidx.compose.ui.text.TextStyle,
    color: androidx.compose.ui.graphics.Color
) {
    val context = LocalContext.current
    val urlPattern = android.util.Patterns.WEB_URL
    val matcher = urlPattern.matcher(text)

    val annotatedString = androidx.compose.ui.text.buildAnnotatedString {
        var lastEnd = 0
        while (matcher.find()) {
            // Add text before the URL
            append(text.substring(lastEnd, matcher.start()))
            // Add the URL with annotation
            val url = matcher.group()
            pushStringAnnotation(tag = "URL", annotation = url)
            withStyle(
                style = androidx.compose.ui.text.SpanStyle(
                    color = MaterialTheme.colorScheme.primary,
                    textDecoration = androidx.compose.ui.text.style.TextDecoration.Underline
                )
            ) {
                append(url)
            }
            pop()
            lastEnd = matcher.end()
        }
        // Add remaining text
        append(text.substring(lastEnd))
    }

    androidx.compose.foundation.text.ClickableText(
        text = annotatedString,
        style = style.copy(color = color),
        onClick = { offset ->
            annotatedString.getStringAnnotations(tag = "URL", start = offset, end = offset)
                .firstOrNull()?.let { annotation ->
                    try {
                        val intent = android.content.Intent(
                            android.content.Intent.ACTION_VIEW,
                            android.net.Uri.parse(annotation.item)
                        )
                        context.startActivity(intent)
                    } catch (e: Exception) { /* ignore */ }
                }
        }
    )
}

private fun formatDate(dateStr: String): String {
    return try {
        val zdt = java.time.ZonedDateTime.parse(dateStr)
        val formatter = java.time.format.DateTimeFormatter.ofPattern("EEE, MMM d, yyyy")
        zdt.format(formatter)
    } catch (e: Exception) {
        dateStr
    }
}

private fun parseDateToMillis(dateStr: String): Long? {
    return try {
        val zdt = java.time.ZonedDateTime.parse(dateStr)
        zdt.toInstant().toEpochMilli()
    } catch (e: Exception) {
        null
    }
}

private fun millisToIsoDate(millis: Long): String {
    val instant = java.time.Instant.ofEpochMilli(millis)
    val zdt = java.time.ZonedDateTime.ofInstant(instant, java.time.ZoneId.systemDefault())
    return zdt.format(java.time.format.DateTimeFormatter.ISO_OFFSET_DATE_TIME)
}
