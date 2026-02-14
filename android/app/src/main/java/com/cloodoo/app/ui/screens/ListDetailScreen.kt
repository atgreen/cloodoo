// ListDetailScreen.kt
//
// SPDX-License-Identifier: MIT
//
// Copyright (C) 2026 Anthony Green <green@moxielogic.com>

package com.cloodoo.app.ui.screens

import android.content.Intent
import androidx.compose.animation.animateColorAsState
import androidx.compose.foundation.ExperimentalFoundationApi
import androidx.compose.foundation.background
import androidx.compose.foundation.layout.*
import androidx.compose.foundation.lazy.LazyColumn
import androidx.compose.foundation.lazy.items
import androidx.compose.material.icons.Icons
import androidx.compose.material.icons.automirrored.filled.ArrowBack
import androidx.compose.material.icons.filled.*
import androidx.compose.material3.*
import androidx.compose.runtime.*
import androidx.compose.ui.Alignment
import androidx.compose.ui.Modifier
import androidx.compose.ui.platform.LocalContext
import androidx.compose.ui.text.style.TextDecoration
import androidx.compose.ui.text.style.TextOverflow
import androidx.compose.ui.unit.dp
import com.cloodoo.app.data.local.ListDefinitionEntity
import com.cloodoo.app.data.local.ListItemEntity
import com.cloodoo.app.ui.navigation.Screen
import androidx.compose.material.icons.filled.CleaningServices
import org.json.JSONArray

@OptIn(ExperimentalMaterial3Api::class, ExperimentalFoundationApi::class)
@Composable
fun ListDetailScreen(
    listId: String,
    viewModel: TodoListViewModel,
    onNavigateBack: () -> Unit,
    onEditList: (String) -> Unit
) {
    val listUiState by viewModel.listUiState.collectAsState()
    val items by viewModel.getListItems(listId).collectAsState(initial = emptyList())
    val context = LocalContext.current

    val listDef = listUiState.lists.find { it.id == listId }

    var showAddItemDialog by remember { mutableStateOf(false) }
    var showDeleteConfirm by remember { mutableStateOf(false) }
    var showClearCheckedConfirm by remember { mutableStateOf(false) }
    var itemToDelete by remember { mutableStateOf<ListItemEntity?>(null) }

    val checkedCount = items.count { it.checked }

    val sections = listDef?.sections?.let { json ->
        try {
            JSONArray(json).let { arr ->
                (0 until arr.length()).map { arr.getString(it) }
            }
        } catch (e: Exception) { emptyList() }
    } ?: emptyList()

    // Group items by section
    val groupedItems = remember(items, sections) {
        val groups = mutableListOf<Pair<String, List<ListItemEntity>>>()
        // Add defined sections in order
        for (section in sections) {
            val sectionItems = items.filter { it.section == section }
            if (sectionItems.isNotEmpty()) {
                groups.add(section to sectionItems)
            }
        }
        // Add unsectioned items
        val unsectioned = items.filter { it.section.isNullOrBlank() || (it.section !in sections) }
        if (unsectioned.isNotEmpty()) {
            val label = if (sections.isNotEmpty()) "Other" else ""
            groups.add(label to unsectioned)
        }
        groups
    }

    Scaffold(
        topBar = {
            TopAppBar(
                title = {
                    Text(
                        text = listDef?.name ?: "List",
                        maxLines = 1,
                        overflow = TextOverflow.Ellipsis
                    )
                },
                navigationIcon = {
                    IconButton(onClick = onNavigateBack) {
                        Icon(Icons.AutoMirrored.Filled.ArrowBack, contentDescription = "Back")
                    }
                },
                actions = {
                    if (checkedCount > 0) {
                        IconButton(onClick = { showClearCheckedConfirm = true }) {
                            Icon(
                                Icons.Default.CleaningServices,
                                contentDescription = "Delete checked items"
                            )
                        }
                    }
                    IconButton(onClick = {
                        // Share list
                        val text = buildShareText(listDef, items, sections)
                        val sendIntent = Intent().apply {
                            action = Intent.ACTION_SEND
                            putExtra(Intent.EXTRA_TEXT, text)
                            type = "text/plain"
                        }
                        context.startActivity(Intent.createChooser(sendIntent, "Share list"))
                    }) {
                        Icon(Icons.Default.Share, contentDescription = "Share")
                    }
                    IconButton(onClick = { onEditList(listId) }) {
                        Icon(Icons.Default.Edit, contentDescription = "Edit list")
                    }
                    IconButton(onClick = { showDeleteConfirm = true }) {
                        Icon(
                            Icons.Default.Delete,
                            contentDescription = "Delete list",
                            tint = MaterialTheme.colorScheme.error
                        )
                    }
                }
            )
        },
        floatingActionButton = {
            FloatingActionButton(onClick = { showAddItemDialog = true }) {
                Icon(Icons.Default.Add, contentDescription = "Add item")
            }
        }
    ) { padding ->
        if (items.isEmpty()) {
            Box(
                modifier = Modifier
                    .fillMaxSize()
                    .padding(padding),
                contentAlignment = Alignment.Center
            ) {
                Column(horizontalAlignment = Alignment.CenterHorizontally) {
                    Text(
                        text = "No items yet",
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
            }
        } else {
            LazyColumn(
                modifier = Modifier
                    .fillMaxSize()
                    .padding(padding),
                contentPadding = PaddingValues(vertical = 8.dp)
            ) {
                groupedItems.forEach { (sectionName, sectionItems) ->
                    if (sectionName.isNotBlank()) {
                        stickyHeader(key = "section_$sectionName") {
                            Surface(
                                modifier = Modifier.fillMaxWidth(),
                                color = MaterialTheme.colorScheme.surface
                            ) {
                                Text(
                                    text = sectionName,
                                    style = MaterialTheme.typography.titleSmall,
                                    color = MaterialTheme.colorScheme.primary,
                                    modifier = Modifier.padding(
                                        horizontal = 16.dp,
                                        vertical = 8.dp
                                    )
                                )
                            }
                        }
                    }
                    items(
                        items = sectionItems,
                        key = { it.rowId }
                    ) { item ->
                        ListItemRow(
                            item = item,
                            onToggle = { viewModel.toggleListItem(item.id) },
                            onDelete = { itemToDelete = item }
                        )
                    }
                }
            }
        }
    }

    // Add item dialog
    if (showAddItemDialog) {
        AddListItemDialog(
            sections = sections,
            onDismiss = { showAddItemDialog = false },
            onAdd = { title, section, notes ->
                viewModel.addListItem(listId, title, section, notes)
                showAddItemDialog = false
            }
        )
    }

    // Delete checked items confirmation
    if (showClearCheckedConfirm) {
        AlertDialog(
            onDismissRequest = { showClearCheckedConfirm = false },
            title = { Text("Delete Checked Items?") },
            text = { Text("Delete $checkedCount checked item${if (checkedCount != 1) "s" else ""}?") },
            confirmButton = {
                TextButton(
                    onClick = {
                        viewModel.deleteCheckedListItems(listId)
                        showClearCheckedConfirm = false
                    },
                    colors = ButtonDefaults.textButtonColors(
                        contentColor = MaterialTheme.colorScheme.error
                    )
                ) {
                    Text("Delete")
                }
            },
            dismissButton = {
                TextButton(onClick = { showClearCheckedConfirm = false }) {
                    Text("Cancel")
                }
            }
        )
    }

    // Delete list confirmation
    if (showDeleteConfirm) {
        AlertDialog(
            onDismissRequest = { showDeleteConfirm = false },
            title = { Text("Delete List?") },
            text = { Text("Delete \"${listDef?.name}\" and all its items?") },
            confirmButton = {
                TextButton(
                    onClick = {
                        viewModel.deleteList(listId)
                        showDeleteConfirm = false
                        onNavigateBack()
                    },
                    colors = ButtonDefaults.textButtonColors(
                        contentColor = MaterialTheme.colorScheme.error
                    )
                ) {
                    Text("Delete")
                }
            },
            dismissButton = {
                TextButton(onClick = { showDeleteConfirm = false }) {
                    Text("Cancel")
                }
            }
        )
    }

    // Delete item confirmation
    itemToDelete?.let { item ->
        AlertDialog(
            onDismissRequest = { itemToDelete = null },
            title = { Text("Delete Item?") },
            text = { Text("Delete \"${item.title}\"?") },
            confirmButton = {
                TextButton(
                    onClick = {
                        viewModel.deleteListItem(item.id)
                        itemToDelete = null
                    },
                    colors = ButtonDefaults.textButtonColors(
                        contentColor = MaterialTheme.colorScheme.error
                    )
                ) {
                    Text("Delete")
                }
            },
            dismissButton = {
                TextButton(onClick = { itemToDelete = null }) {
                    Text("Cancel")
                }
            }
        )
    }
}

@Composable
private fun ListItemRow(
    item: ListItemEntity,
    onToggle: () -> Unit,
    onDelete: () -> Unit
) {
    val textColor by animateColorAsState(
        targetValue = if (item.checked)
            MaterialTheme.colorScheme.onSurfaceVariant
        else
            MaterialTheme.colorScheme.onSurface,
        label = "textColor"
    )

    Row(
        modifier = Modifier
            .fillMaxWidth()
            .padding(horizontal = 8.dp, vertical = 2.dp),
        verticalAlignment = Alignment.CenterVertically
    ) {
        Checkbox(
            checked = item.checked,
            onCheckedChange = { onToggle() }
        )
        Column(modifier = Modifier.weight(1f)) {
            Text(
                text = item.title,
                style = MaterialTheme.typography.bodyLarge,
                color = textColor,
                textDecoration = if (item.checked) TextDecoration.LineThrough else TextDecoration.None,
                maxLines = 2,
                overflow = TextOverflow.Ellipsis
            )
            if (!item.notes.isNullOrBlank()) {
                Text(
                    text = item.notes,
                    style = MaterialTheme.typography.bodySmall,
                    color = MaterialTheme.colorScheme.onSurfaceVariant,
                    maxLines = 1,
                    overflow = TextOverflow.Ellipsis
                )
            }
        }
        IconButton(onClick = onDelete) {
            Icon(
                Icons.Default.Close,
                contentDescription = "Delete item",
                modifier = Modifier.size(18.dp),
                tint = MaterialTheme.colorScheme.onSurfaceVariant
            )
        }
    }
}

@Composable
private fun AddListItemDialog(
    sections: List<String>,
    onDismiss: () -> Unit,
    onAdd: (title: String, section: String?, notes: String?) -> Unit
) {
    var title by remember { mutableStateOf("") }
    var selectedSection by remember { mutableStateOf<String?>(null) }
    var notes by remember { mutableStateOf("") }

    AlertDialog(
        onDismissRequest = onDismiss,
        title = { Text("Add Item") },
        text = {
            Column(verticalArrangement = Arrangement.spacedBy(12.dp)) {
                OutlinedTextField(
                    value = title,
                    onValueChange = { title = it },
                    label = { Text("Item title") },
                    singleLine = true,
                    modifier = Modifier.fillMaxWidth()
                )
                if (sections.isNotEmpty()) {
                    Text("Section", style = MaterialTheme.typography.labelMedium)
                    Row(
                        horizontalArrangement = Arrangement.spacedBy(8.dp),
                        modifier = Modifier.fillMaxWidth()
                    ) {
                        FilterChip(
                            selected = selectedSection == null,
                            onClick = { selectedSection = null },
                            label = { Text("None") }
                        )
                    }
                    sections.forEach { section ->
                        FilterChip(
                            selected = selectedSection == section,
                            onClick = { selectedSection = section },
                            label = { Text(section) }
                        )
                    }
                }
                OutlinedTextField(
                    value = notes,
                    onValueChange = { notes = it },
                    label = { Text("Notes (optional)") },
                    modifier = Modifier.fillMaxWidth(),
                    maxLines = 3
                )
            }
        },
        confirmButton = {
            TextButton(
                onClick = {
                    onAdd(title, selectedSection, notes.ifBlank { null })
                },
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

private fun buildShareText(
    listDef: ListDefinitionEntity?,
    items: List<ListItemEntity>,
    sections: List<String>
): String {
    val sb = StringBuilder()
    sb.appendLine(listDef?.name ?: "List")
    sb.appendLine()

    // Group items by section
    for (section in sections) {
        val sectionItems = items.filter { it.section == section }
        if (sectionItems.isNotEmpty()) {
            sb.appendLine(section)
            for (item in sectionItems) {
                val check = if (item.checked) "x" else " "
                sb.appendLine("  [$check] ${item.title}")
            }
        }
    }

    // Unsectioned
    val unsectioned = items.filter { it.section.isNullOrBlank() || (it.section !in sections) }
    if (unsectioned.isNotEmpty()) {
        if (sections.isNotEmpty()) {
            sb.appendLine("Other")
        }
        for (item in unsectioned) {
            val check = if (item.checked) "x" else " "
            sb.appendLine("  [$check] ${item.title}")
        }
    }

    return sb.toString().trimEnd()
}
