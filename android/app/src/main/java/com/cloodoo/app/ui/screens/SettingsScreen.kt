// SettingsScreen.kt
//
// SPDX-License-Identifier: MIT
//
// Copyright (C) 2026 Anthony Green <green@moxielogic.com>

package com.cloodoo.app.ui.screens

import androidx.compose.foundation.clickable
import androidx.compose.foundation.layout.*
import androidx.compose.foundation.rememberScrollState
import androidx.compose.foundation.verticalScroll
import androidx.compose.material.icons.Icons
import androidx.compose.material.icons.automirrored.filled.ArrowBack
import androidx.compose.material.icons.filled.Cloud
import androidx.compose.material.icons.filled.CloudOff
import androidx.compose.material3.*
import androidx.compose.runtime.*
import androidx.compose.ui.Alignment
import androidx.compose.ui.Modifier
import androidx.compose.ui.unit.dp
import com.cloodoo.app.data.remote.ConnectionState
import com.cloodoo.app.data.security.CertificateManager

@OptIn(ExperimentalMaterial3Api::class)
@Composable
fun SettingsScreen(
    viewModel: TodoListViewModel,
    certificateManager: CertificateManager,
    onUnpaired: () -> Unit,
    onPairNewDevice: () -> Unit = {}
) {
    var serverAddress by remember { mutableStateOf(certificateManager.getServerAddress() ?: "") }
    var serverPort by remember { mutableStateOf(certificateManager.getServerPort().toString()) }
    var showUnpairConfirmation by remember { mutableStateOf(false) }
    var showContextEditor by remember { mutableStateOf(false) }

    val connectionState by viewModel.connectionState.collectAsState()
    val userContext by viewModel.userContext.collectAsState()
    val deviceName = certificateManager.getDeviceName() ?: "Unknown"

    Column(
        modifier = Modifier
            .fillMaxSize()
            .padding(16.dp),
        verticalArrangement = Arrangement.spacedBy(24.dp)
    ) {
        // Server section
        Text(
            text = "Server",
            style = MaterialTheme.typography.titleMedium,
            color = MaterialTheme.colorScheme.primary
        )
        Card(modifier = Modifier.fillMaxWidth()) {
            Column(
                modifier = Modifier.padding(16.dp),
                verticalArrangement = Arrangement.spacedBy(12.dp)
            ) {
                OutlinedTextField(
                    value = serverAddress,
                    onValueChange = { serverAddress = it },
                    label = { Text("Server Address") },
                    singleLine = true,
                    modifier = Modifier.fillMaxWidth()
                )
                OutlinedTextField(
                    value = serverPort,
                    onValueChange = { serverPort = it.filter { c -> c.isDigit() } },
                    label = { Text("Port") },
                    singleLine = true,
                    modifier = Modifier.fillMaxWidth()
                )
                Button(
                    onClick = {
                        val port = serverPort.toIntOrNull() ?: 50051
                        certificateManager.updateServerConfig(serverAddress, port)
                        viewModel.refreshSync()
                    },
                    enabled = serverAddress.isNotBlank() && serverPort.isNotBlank(),
                    modifier = Modifier.fillMaxWidth()
                ) {
                    Text("Save & Reconnect")
                }
            }
        }

        // Connection section
        Text(
            text = "Connection",
            style = MaterialTheme.typography.titleMedium,
            color = MaterialTheme.colorScheme.primary
        )
        Card(modifier = Modifier.fillMaxWidth()) {
            Column(
                modifier = Modifier.padding(16.dp),
                verticalArrangement = Arrangement.spacedBy(12.dp)
            ) {
                Row(
                    verticalAlignment = Alignment.CenterVertically,
                    horizontalArrangement = Arrangement.spacedBy(8.dp)
                ) {
                    when (connectionState) {
                        ConnectionState.CONNECTED -> {
                            Icon(
                                Icons.Default.Cloud,
                                contentDescription = null,
                                tint = MaterialTheme.colorScheme.primary
                            )
                            Text("Connected", color = MaterialTheme.colorScheme.primary)
                        }
                        ConnectionState.CONNECTING -> {
                            CircularProgressIndicator(
                                modifier = Modifier.size(20.dp),
                                strokeWidth = 2.dp
                            )
                            Text("Connecting...")
                        }
                        ConnectionState.ERROR -> {
                            Icon(
                                Icons.Default.CloudOff,
                                contentDescription = null,
                                tint = MaterialTheme.colorScheme.error
                            )
                            Text("Error", color = MaterialTheme.colorScheme.error)
                        }
                        ConnectionState.DISCONNECTED -> {
                            Icon(
                                Icons.Default.CloudOff,
                                contentDescription = null,
                                tint = MaterialTheme.colorScheme.onSurfaceVariant
                            )
                            Text("Disconnected", color = MaterialTheme.colorScheme.onSurfaceVariant)
                        }
                    }
                }

                HorizontalDivider()

                Row(
                    modifier = Modifier.fillMaxWidth(),
                    horizontalArrangement = Arrangement.SpaceBetween
                ) {
                    Text("Device", style = MaterialTheme.typography.bodyMedium)
                    Text(
                        deviceName,
                        style = MaterialTheme.typography.bodyMedium,
                        color = MaterialTheme.colorScheme.onSurfaceVariant
                    )
                }
            }
        }

        // Enrichment section
        Text(
            text = "Enrichment",
            style = MaterialTheme.typography.titleMedium,
            color = MaterialTheme.colorScheme.primary
        )
        Card(
            modifier = Modifier
                .fillMaxWidth()
                .clickable { showContextEditor = true }
        ) {
            Column(
                modifier = Modifier.padding(16.dp),
                verticalArrangement = Arrangement.spacedBy(8.dp)
            ) {
                Row(
                    modifier = Modifier.fillMaxWidth(),
                    horizontalArrangement = Arrangement.SpaceBetween,
                    verticalAlignment = Alignment.CenterVertically
                ) {
                    Text(
                        text = "User Context",
                        style = MaterialTheme.typography.bodyLarge
                    )
                    Text(
                        text = "Tap to edit",
                        style = MaterialTheme.typography.bodySmall,
                        color = MaterialTheme.colorScheme.primary
                    )
                }
                Text(
                    text = if (userContext.isBlank()) {
                        "Not set - tap to add context for LLM enrichment"
                    } else {
                        val preview = userContext.take(100)
                        if (userContext.length > 100) "$preview..." else preview
                    },
                    style = MaterialTheme.typography.bodyMedium,
                    color = MaterialTheme.colorScheme.onSurfaceVariant
                )
                if (userContext.isNotBlank()) {
                    Text(
                        text = "${userContext.length} characters",
                        style = MaterialTheme.typography.bodySmall,
                        color = MaterialTheme.colorScheme.outline
                    )
                }
            }
        }

        // Account section
        Text(
            text = "Account",
            style = MaterialTheme.typography.titleMedium,
            color = MaterialTheme.colorScheme.primary
        )
        Card(modifier = Modifier.fillMaxWidth()) {
            Column(modifier = Modifier.padding(16.dp)) {
                Button(
                    onClick = onPairNewDevice,
                    modifier = Modifier.fillMaxWidth()
                ) {
                    Text("Pair New Device")
                }
                Spacer(modifier = Modifier.height(8.dp))
                OutlinedButton(
                    onClick = { showUnpairConfirmation = true },
                    colors = ButtonDefaults.outlinedButtonColors(
                        contentColor = MaterialTheme.colorScheme.error
                    ),
                    modifier = Modifier.fillMaxWidth()
                ) {
                    Text("Unpair Device")
                }
            }
        }
    }

    // Unpair confirmation dialog
    if (showUnpairConfirmation) {
        AlertDialog(
            onDismissRequest = { showUnpairConfirmation = false },
            title = { Text("Unpair Device?") },
            text = { Text("This will remove the certificate and disconnect from the server. You will need to pair again to use the app.") },
            confirmButton = {
                TextButton(
                    onClick = {
                        viewModel.unpair()
                        showUnpairConfirmation = false
                        onUnpaired()
                    },
                    colors = ButtonDefaults.textButtonColors(
                        contentColor = MaterialTheme.colorScheme.error
                    )
                ) {
                    Text("Unpair")
                }
            },
            dismissButton = {
                TextButton(onClick = { showUnpairConfirmation = false }) {
                    Text("Cancel")
                }
            }
        )
    }

    // User Context Editor Dialog
    if (showContextEditor) {
        UserContextEditorDialog(
            initialContext = userContext,
            onDismiss = { showContextEditor = false },
            onSave = { newContext ->
                viewModel.saveUserContext(newContext)
                showContextEditor = false
            }
        )
    }
}

@OptIn(ExperimentalMaterial3Api::class)
@Composable
fun UserContextEditorDialog(
    initialContext: String,
    onDismiss: () -> Unit,
    onSave: (String) -> Unit
) {
    var editedContext by remember { mutableStateOf(initialContext) }
    var showUnsavedWarning by remember { mutableStateOf(false) }
    val hasUnsavedChanges = editedContext != initialContext

    fun handleDismiss() {
        if (hasUnsavedChanges) {
            showUnsavedWarning = true
        } else {
            onDismiss()
        }
    }

    androidx.compose.ui.window.Dialog(
        onDismissRequest = { handleDismiss() },
        properties = androidx.compose.ui.window.DialogProperties(
            usePlatformDefaultWidth = false,
            dismissOnBackPress = false,
            decorFitsSystemWindows = false
        )
    ) {
        Scaffold(
            modifier = Modifier
                .fillMaxSize()
                .systemBarsPadding()
                .imePadding(),
            topBar = {
                TopAppBar(
                    title = { Text("User Context") },
                    navigationIcon = {
                        IconButton(onClick = { handleDismiss() }) {
                            Icon(
                                imageVector = Icons.AutoMirrored.Filled.ArrowBack,
                                contentDescription = "Cancel"
                            )
                        }
                    },
                    actions = {
                        TextButton(
                            onClick = { onSave(editedContext) },
                            enabled = hasUnsavedChanges
                        ) {
                            Text("SAVE")
                        }
                    },
                    colors = TopAppBarDefaults.topAppBarColors(
                        containerColor = MaterialTheme.colorScheme.surface
                    )
                )
            }
        ) { paddingValues ->
            OutlinedTextField(
                value = editedContext,
                onValueChange = { editedContext = it },
                modifier = Modifier
                    .fillMaxSize()
                    .padding(paddingValues)
                    .padding(16.dp),
                placeholder = { Text("Enter context for LLM enrichment...\n\nExample:\n- I'm a software developer\n- I prefer concise descriptions\n- Tag work items with 'work'") },
                textStyle = MaterialTheme.typography.bodyMedium,
                colors = OutlinedTextFieldDefaults.colors(
                    unfocusedContainerColor = MaterialTheme.colorScheme.surface,
                    focusedContainerColor = MaterialTheme.colorScheme.surface
                )
            )
        }
    }

    // Unsaved changes warning dialog
    if (showUnsavedWarning) {
        AlertDialog(
            onDismissRequest = { showUnsavedWarning = false },
            title = { Text("Unsaved Changes") },
            text = { Text("You have unsaved changes. Discard them?") },
            confirmButton = {
                TextButton(
                    onClick = {
                        showUnsavedWarning = false
                        onDismiss()
                    }
                ) {
                    Text("Discard")
                }
            },
            dismissButton = {
                TextButton(onClick = { showUnsavedWarning = false }) {
                    Text("Cancel")
                }
            }
        )
    }
}
