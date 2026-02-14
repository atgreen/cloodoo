// CreateListScreen.kt
//
// SPDX-License-Identifier: MIT
//
// Copyright (C) 2026 Anthony Green <green@moxielogic.com>

package com.cloodoo.app.ui.screens

import androidx.compose.foundation.layout.*
import androidx.compose.foundation.rememberScrollState
import androidx.compose.foundation.verticalScroll
import androidx.compose.material.icons.Icons
import androidx.compose.material.icons.filled.Add
import androidx.compose.material.icons.filled.Close
import androidx.compose.material.icons.filled.Delete
import androidx.compose.material3.*
import androidx.compose.runtime.*
import androidx.compose.ui.Alignment
import androidx.compose.ui.Modifier
import androidx.compose.ui.focus.FocusRequester
import androidx.compose.ui.focus.focusRequester
import androidx.compose.ui.unit.dp
import com.cloodoo.app.data.local.ListDefinitionEntity
import org.json.JSONArray

@OptIn(ExperimentalMaterial3Api::class)
@Composable
fun CreateListScreen(
    viewModel: TodoListViewModel,
    onNavigateBack: () -> Unit,
    existingListId: String? = null
) {
    val listUiState by viewModel.listUiState.collectAsState()
    val existingList = existingListId?.let { id -> listUiState.lists.find { it.id == id } }
    val isEdit = existingList != null

    val existingSections = existingList?.sections?.let { json ->
        try {
            JSONArray(json).let { arr ->
                (0 until arr.length()).map { arr.getString(it) }
            }
        } catch (e: Exception) { emptyList() }
    } ?: emptyList()

    var name by remember { mutableStateOf(existingList?.name ?: "") }
    var description by remember { mutableStateOf(existingList?.description ?: "") }
    var sections by remember { mutableStateOf(existingSections) }
    var newSectionText by remember { mutableStateOf("") }

    val focusRequester = remember { FocusRequester() }
    LaunchedEffect(Unit) { focusRequester.requestFocus() }

    Scaffold(
        topBar = {
            TopAppBar(
                title = { Text(if (isEdit) "Edit List" else "New List") },
                navigationIcon = {
                    IconButton(onClick = onNavigateBack) {
                        Icon(Icons.Default.Close, contentDescription = "Close")
                    }
                },
                actions = {
                    TextButton(
                        onClick = {
                            val sectionsJson = if (sections.isNotEmpty()) {
                                JSONArray(sections).toString()
                            } else null

                            if (isEdit) {
                                viewModel.updateList(
                                    id = existingListId!!,
                                    name = name.trim(),
                                    description = description.ifBlank { null },
                                    sections = sectionsJson
                                )
                            } else {
                                viewModel.createList(
                                    name = name.trim(),
                                    description = description.ifBlank { null },
                                    sections = sectionsJson
                                )
                            }
                            onNavigateBack()
                        },
                        enabled = name.isNotBlank()
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
            // Name
            OutlinedTextField(
                value = name,
                onValueChange = { name = it },
                label = { Text("List name") },
                singleLine = true,
                modifier = Modifier
                    .fillMaxWidth()
                    .focusRequester(focusRequester)
            )

            // Description
            OutlinedTextField(
                value = description,
                onValueChange = { description = it },
                label = { Text("Description (optional)") },
                modifier = Modifier
                    .fillMaxWidth()
                    .heightIn(min = 80.dp),
                maxLines = 4
            )

            // Sections
            Text("Sections", style = MaterialTheme.typography.labelLarge)
            Text(
                text = "Organize items into categories (e.g. Produce, Dairy, Frozen)",
                style = MaterialTheme.typography.bodySmall,
                color = MaterialTheme.colorScheme.onSurfaceVariant
            )

            sections.forEachIndexed { index, section ->
                Row(
                    verticalAlignment = Alignment.CenterVertically,
                    horizontalArrangement = Arrangement.spacedBy(8.dp)
                ) {
                    OutlinedTextField(
                        value = section,
                        onValueChange = { newValue ->
                            sections = sections.toMutableList().apply {
                                set(index, newValue)
                            }
                        },
                        singleLine = true,
                        modifier = Modifier.weight(1f),
                        textStyle = MaterialTheme.typography.bodyMedium
                    )
                    IconButton(
                        onClick = {
                            sections = sections.toMutableList().apply { removeAt(index) }
                        }
                    ) {
                        Icon(
                            Icons.Default.Delete,
                            contentDescription = "Remove section",
                            tint = MaterialTheme.colorScheme.error
                        )
                    }
                }
            }

            // Add section
            Row(
                verticalAlignment = Alignment.CenterVertically,
                horizontalArrangement = Arrangement.spacedBy(8.dp)
            ) {
                OutlinedTextField(
                    value = newSectionText,
                    onValueChange = { newSectionText = it },
                    placeholder = { Text("Add section") },
                    singleLine = true,
                    modifier = Modifier.weight(1f),
                    textStyle = MaterialTheme.typography.bodyMedium
                )
                IconButton(
                    onClick = {
                        val trimmed = newSectionText.trim()
                        if (trimmed.isNotEmpty() && trimmed !in sections) {
                            sections = sections + trimmed
                            newSectionText = ""
                        }
                    },
                    enabled = newSectionText.isNotBlank()
                ) {
                    Icon(Icons.Default.Add, contentDescription = "Add section")
                }
            }
        }
    }
}
