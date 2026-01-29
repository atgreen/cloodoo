// InboxScreen.kt
//
// SPDX-License-Identifier: MIT
//
// Copyright (C) 2026 Anthony Green <green@moxielogic.com>

package com.cloodoo.app.ui.screens

import androidx.compose.animation.AnimatedVisibility
import androidx.compose.foundation.ExperimentalFoundationApi
import androidx.compose.foundation.horizontalScroll
import androidx.compose.foundation.layout.*
import androidx.compose.foundation.lazy.LazyColumn
import androidx.compose.foundation.lazy.items
import androidx.compose.foundation.rememberScrollState
import androidx.compose.material.icons.Icons
import androidx.compose.material.icons.filled.Clear
import androidx.compose.material.icons.filled.Search
import androidx.compose.material3.*
import androidx.compose.runtime.*
import androidx.compose.ui.Alignment
import androidx.compose.ui.Modifier
import androidx.compose.ui.focus.FocusRequester
import androidx.compose.ui.focus.focusRequester
import androidx.compose.ui.unit.dp
import com.cloodoo.app.data.local.TodoEntity
import com.cloodoo.app.ui.components.DateGroup
import com.cloodoo.app.ui.components.SectionHeader
import com.cloodoo.app.ui.components.SwipeableTodoItem
import com.cloodoo.app.ui.util.parseTags

@OptIn(ExperimentalMaterial3Api::class, ExperimentalFoundationApi::class)
@Composable
fun InboxScreen(
    viewModel: TodoListViewModel
) {
    val uiState by viewModel.uiState.collectAsState()
    var searchQuery by remember { mutableStateOf("") }
    var isSearchVisible by remember { mutableStateOf(false) }
    var selectedTag by remember { mutableStateOf<String?>(null) }
    var selectedTodo by remember { mutableStateOf<TodoEntity?>(null) }
    var todoToConfirmComplete by remember { mutableStateOf<TodoEntity?>(null) }
    var todoToConfirmCancel by remember { mutableStateOf<TodoEntity?>(null) }

    val allTags = remember(uiState.todos) {
        uiState.todos
            .filter { it.status != "completed" && it.status != "cancelled" }
            .flatMap { todo -> parseTags(todo.tags) }
            .distinct()
            .sorted()
    }

    val activeGroupedTodos = remember(uiState.groupedTodos, searchQuery, selectedTag) {
        val activeGroups = uiState.groupedTodos.filter { it.group != DateGroup.COMPLETED }
        val currentTag = selectedTag
        if (searchQuery.isBlank() && currentTag == null) {
            activeGroups
        } else {
            activeGroups.mapNotNull { group ->
                val filteredTodos = group.todos.filter { todo ->
                    val matchesSearch = searchQuery.isBlank() ||
                        todo.title.contains(searchQuery, ignoreCase = true) ||
                        (todo.description?.contains(searchQuery, ignoreCase = true) == true)
                    val matchesTag = currentTag == null ||
                        (todo.tags?.contains(currentTag) == true)
                    matchesSearch && matchesTag
                }
                if (filteredTodos.isNotEmpty()) group.copy(todos = filteredTodos) else null
            }
        }
    }

    Column(modifier = Modifier.fillMaxSize()) {
        // Sync progress indicator
        if (uiState.isSyncing) {
            LinearProgressIndicator(modifier = Modifier.fillMaxWidth())
        }

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

        // Search toggle + tag filter chips
        Row(
            modifier = Modifier
                .fillMaxWidth()
                .padding(horizontal = 4.dp, vertical = 4.dp),
            verticalAlignment = Alignment.CenterVertically
        ) {
            IconButton(onClick = { isSearchVisible = !isSearchVisible }) {
                Icon(Icons.Default.Search, contentDescription = "Search")
            }
            if (allTags.isNotEmpty()) {
                Row(
                    modifier = Modifier
                        .horizontalScroll(rememberScrollState())
                        .weight(1f),
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
        }

        // Content
        Box(modifier = Modifier.weight(1f).fillMaxWidth()) {
            if (uiState.isLoading) {
                CircularProgressIndicator(modifier = Modifier.align(Alignment.Center))
            } else if (activeGroupedTodos.isEmpty() && searchQuery.isBlank() && selectedTag == null) {
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
            } else if (activeGroupedTodos.isEmpty()) {
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
                    activeGroupedTodos.forEach { groupData ->
                        stickyHeader(key = "header_${groupData.group.name}") {
                            SectionHeader(
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
                                SwipeableTodoItem(
                                    todo = todo,
                                    onToggleComplete = { todoId ->
                                        todoToConfirmComplete = uiState.todos.find { t -> t.id == todoId }
                                    },
                                    onClick = { selectedTodo = uiState.todos.find { t -> t.id == it } },
                                    onCancel = { todoId ->
                                        todoToConfirmCancel = uiState.todos.find { t -> t.id == todoId }
                                    }
                                )
                            }
                        }
                    }
                }
            }
        }
    }

    // Keep selectedTodo fresh
    LaunchedEffect(uiState.todos) {
        selectedTodo?.let { sel ->
            selectedTodo = uiState.todos.find { it.id == sel.id }
        }
    }

    // Detail bottom sheet
    if (selectedTodo != null) {
        TodoDetailSheet(
            todo = selectedTodo!!,
            onDismiss = { selectedTodo = null },
            onUpdate = { todoId, dueDate, scheduledDate, tags ->
                viewModel.updateTodo(todoId, dueDate = dueDate, scheduledDate = scheduledDate, tags = tags)
            }
        )
    }

    // Cancel confirmation dialog
    todoToConfirmCancel?.let { todo ->
        AlertDialog(
            onDismissRequest = { todoToConfirmCancel = null },
            title = { Text("Cancel Task?") },
            text = { Text("Cancel \"${todo.title}\"? You can reopen it later from Completed.") },
            confirmButton = {
                TextButton(
                    onClick = {
                        viewModel.cancelTodo(todo.id)
                        todoToConfirmCancel = null
                    },
                    colors = ButtonDefaults.textButtonColors(
                        contentColor = MaterialTheme.colorScheme.error
                    )
                ) {
                    Text("Cancel Task")
                }
            },
            dismissButton = {
                TextButton(onClick = { todoToConfirmCancel = null }) {
                    Text("Keep")
                }
            }
        )
    }

    // Completion confirmation dialog
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
