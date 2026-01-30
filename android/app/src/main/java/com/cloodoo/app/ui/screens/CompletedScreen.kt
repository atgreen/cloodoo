// CompletedScreen.kt
//
// SPDX-License-Identifier: MIT
//
// Copyright (C) 2026 Anthony Green <green@moxielogic.com>

package com.cloodoo.app.ui.screens

import androidx.compose.foundation.layout.*
import androidx.compose.foundation.lazy.LazyColumn
import androidx.compose.foundation.lazy.items
import androidx.compose.material3.*
import androidx.compose.runtime.*
import androidx.compose.ui.Alignment
import androidx.compose.ui.Modifier
import androidx.compose.ui.unit.dp
import androidx.navigation.NavController
import com.cloodoo.app.data.local.TodoEntity
import com.cloodoo.app.ui.components.DateGroup
import com.cloodoo.app.ui.components.SwipeableTodoItem

@Composable
fun CompletedScreen(
    viewModel: TodoListViewModel,
    navController: NavController
) {
    val uiState by viewModel.uiState.collectAsState()

    var selectedTodo by remember { mutableStateOf<TodoEntity?>(null) }
    var todoToConfirmReopen by remember { mutableStateOf<TodoEntity?>(null) }

    val completedTodos = remember(uiState.groupedTodos) {
        uiState.groupedTodos
            .filter { it.group == DateGroup.COMPLETED }
            .flatMap { it.todos }
    }

    Column(modifier = Modifier.fillMaxSize()) {
        // Sync progress indicator
        if (uiState.isSyncing) {
            LinearProgressIndicator(modifier = Modifier.fillMaxWidth())
        }

        Box(modifier = Modifier.weight(1f)) {
            if (completedTodos.isEmpty()) {
                Column(
                    modifier = Modifier.align(Alignment.Center),
                    horizontalAlignment = Alignment.CenterHorizontally
                ) {
                    Text(
                        text = "No completed tasks yet",
                        style = MaterialTheme.typography.titleMedium,
                        color = MaterialTheme.colorScheme.onSurfaceVariant
                    )
                }
            } else {
                LazyColumn(
                    modifier = Modifier.fillMaxSize(),
                    contentPadding = PaddingValues(vertical = 8.dp)
                ) {
                    items(
                        items = completedTodos,
                        key = { it.rowId }
                    ) { todo ->
                        SwipeableTodoItem(
                            todo = todo,
                            onToggleComplete = { todoId ->
                                todoToConfirmReopen = uiState.todos.find { t -> t.id == todoId }
                            },
                            onClick = { selectedTodo = uiState.todos.find { t -> t.id == it } },
                            onCancel = { },
                            enableCancel = false
                        )
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

    // Reopen confirmation dialog
    todoToConfirmReopen?.let { todo ->
        AlertDialog(
            onDismissRequest = { todoToConfirmReopen = null },
            title = { Text("Reopen Task?") },
            text = { Text("Move \"${todo.title}\" back to the inbox?") },
            confirmButton = {
                TextButton(
                    onClick = {
                        viewModel.toggleComplete(todo.id)
                        todoToConfirmReopen = null
                    }
                ) {
                    Text("Reopen")
                }
            },
            dismissButton = {
                TextButton(onClick = { todoToConfirmReopen = null }) {
                    Text("Cancel")
                }
            }
        )
    }

    // Detail bottom sheet
    if (selectedTodo != null) {
        TodoDetailSheet(
            todo = selectedTodo!!,
            onDismiss = { selectedTodo = null },
            onUpdate = { todoId, dueDate, scheduledDate, tags ->
                viewModel.updateTodo(todoId, dueDate = dueDate, scheduledDate = scheduledDate, tags = tags)
            },
            onEdit = { todoId ->
                navController.navigate(com.cloodoo.app.ui.navigation.Screen.EditTask.createRoute(todoId))
            }
        )
    }
}
