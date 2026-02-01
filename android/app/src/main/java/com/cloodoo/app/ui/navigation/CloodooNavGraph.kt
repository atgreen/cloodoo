// CloodooNavGraph.kt
//
// SPDX-License-Identifier: MIT
//
// Copyright (C) 2026 Anthony Green <green@moxielogic.com>

package com.cloodoo.app.ui.navigation

import android.app.Application
import androidx.compose.foundation.layout.*
import androidx.compose.material.icons.Icons
import androidx.compose.material.icons.filled.*
import androidx.compose.material.icons.outlined.DoneAll
import androidx.compose.material.icons.outlined.Inbox
import androidx.compose.material.icons.outlined.Settings
import androidx.compose.material3.*
import androidx.compose.runtime.*
import androidx.compose.ui.Alignment
import androidx.compose.ui.Modifier
import androidx.compose.ui.platform.LocalContext
import androidx.compose.ui.text.font.FontWeight
import androidx.compose.ui.unit.dp
import java.time.LocalDate
import java.time.format.DateTimeFormatter
import java.util.Locale
import androidx.lifecycle.viewmodel.compose.viewModel
import androidx.navigation.compose.NavHost
import androidx.navigation.compose.composable
import androidx.navigation.compose.currentBackStackEntryAsState
import androidx.navigation.compose.rememberNavController
import com.cloodoo.app.data.remote.ConnectionState
import com.cloodoo.app.data.security.CertificateManager
import com.cloodoo.app.ui.components.ConfettiOverlay
import com.cloodoo.app.ui.components.SpeedDialFab
import com.cloodoo.app.ui.screens.*

@OptIn(ExperimentalMaterial3Api::class)
@Composable
fun CloodooApp(openQuickAdd: Boolean = false) {
    val context = LocalContext.current
    val application = context.applicationContext as Application
    val certificateManager = remember { CertificateManager(context) }
    var isPaired by remember { mutableStateOf(certificateManager.hasCertificate()) }

    if (!isPaired) {
        PairingScreen(
            certificateManager = certificateManager,
            onPairingComplete = { isPaired = true }
        )
    } else {
        val navController = rememberNavController()

        LaunchedEffect(openQuickAdd) {
            if (openQuickAdd) {
                navController.navigate(Screen.QuickAdd.route)
            }
        }

        val viewModel: TodoListViewModel = viewModel(
            factory = TodoListViewModel.Factory(application, certificateManager)
        )
        val uiState by viewModel.uiState.collectAsState()
        val connectionState by viewModel.connectionState.collectAsState()
        val snackbarHostState = remember { SnackbarHostState() }

        val navBackStackEntry by navController.currentBackStackEntryAsState()
        val currentRoute = navBackStackEntry?.destination?.route

        val showBottomBar = currentRoute in listOf(
            Screen.Inbox.route, Screen.Completed.route, Screen.Settings.route
        )
        val showFab = currentRoute in listOf(Screen.Inbox.route, Screen.Completed.route)

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

        Box(modifier = Modifier.fillMaxSize()) {
            Scaffold(
                topBar = {
                    if (showBottomBar) {
                        TopAppBar(
                            title = {
                                Row(
                                    verticalAlignment = Alignment.CenterVertically,
                                    horizontalArrangement = Arrangement.Start
                                ) {
                                    Icon(
                                        imageVector = Icons.Default.Check,
                                        contentDescription = "Cloodoo",
                                        modifier = Modifier.padding(end = 12.dp)
                                    )
                                    Column {
                                        Text(
                                            text = LocalDate.now().format(
                                                DateTimeFormatter.ofPattern("EEEE, MMMM d", Locale.getDefault())
                                            ),
                                            style = MaterialTheme.typography.titleMedium,
                                            fontWeight = FontWeight.Medium
                                        )
                                    }
                                }
                            },
                            actions = {
                                IconButton(onClick = { viewModel.connect() }) {
                                    when (connectionState) {
                                        ConnectionState.CONNECTED -> Icon(
                                            Icons.Default.Cloud,
                                            contentDescription = "Connected",
                                            tint = MaterialTheme.colorScheme.primary
                                        )
                                        ConnectionState.CONNECTING -> CircularProgressIndicator(
                                            modifier = Modifier.size(24.dp),
                                            strokeWidth = 2.dp
                                        )
                                        ConnectionState.ERROR -> Icon(
                                            Icons.Default.CloudOff,
                                            contentDescription = "Error - tap to reconnect",
                                            tint = MaterialTheme.colorScheme.error
                                        )
                                        ConnectionState.DISCONNECTED -> Icon(
                                            Icons.Default.CloudOff,
                                            contentDescription = "Disconnected - tap to reconnect",
                                            tint = MaterialTheme.colorScheme.onSurfaceVariant
                                        )
                                    }
                                }
                            }
                        )
                    }
                },
                bottomBar = {
                    if (showBottomBar) {
                        NavigationBar {
                            NavigationBarItem(
                                selected = currentRoute == Screen.Inbox.route,
                                onClick = {
                                    navController.navigate(Screen.Inbox.route) {
                                        popUpTo(Screen.Inbox.route) { inclusive = true }
                                        launchSingleTop = true
                                    }
                                },
                                icon = { Icon(Icons.Outlined.Inbox, contentDescription = "Inbox") },
                                label = { Text("Inbox") }
                            )
                            NavigationBarItem(
                                selected = currentRoute == Screen.Completed.route,
                                onClick = {
                                    navController.navigate(Screen.Completed.route) {
                                        popUpTo(Screen.Inbox.route) { saveState = true }
                                        launchSingleTop = true
                                        restoreState = true
                                    }
                                },
                                icon = { Icon(Icons.Outlined.DoneAll, contentDescription = "Completed") },
                                label = { Text("Completed") }
                            )
                            NavigationBarItem(
                                selected = currentRoute == Screen.Settings.route,
                                onClick = {
                                    navController.navigate(Screen.Settings.route) {
                                        popUpTo(Screen.Inbox.route) { saveState = true }
                                        launchSingleTop = true
                                        restoreState = true
                                    }
                                },
                                icon = { Icon(Icons.Outlined.Settings, contentDescription = "Settings") },
                                label = { Text("Settings") }
                            )
                        }
                    }
                },
                floatingActionButton = {},
                snackbarHost = { SnackbarHost(snackbarHostState) }
            ) { padding ->
                NavHost(
                    navController = navController,
                    startDestination = Screen.Inbox.route,
                    modifier = Modifier.padding(padding)
                ) {
                    composable(Screen.Inbox.route) {
                        InboxScreen(viewModel = viewModel, navController = navController)
                    }
                    composable(Screen.Completed.route) {
                        CompletedScreen(viewModel = viewModel, navController = navController)
                    }
                    composable(Screen.Settings.route) {
                        SettingsScreen(
                            viewModel = viewModel,
                            certificateManager = certificateManager,
                            onUnpaired = { isPaired = false }
                        )
                    }
                    composable(Screen.AddTask.route) {
                        AddTaskScreen(
                            viewModel = viewModel,
                            onNavigateBack = { navController.popBackStack() }
                        )
                    }
                    composable(Screen.EditTask.route) { backStackEntry ->
                        val todoId = backStackEntry.arguments?.getString("todoId") ?: return@composable
                        EditTaskScreen(
                            todoId = todoId,
                            viewModel = viewModel,
                            onNavigateBack = { navController.popBackStack() }
                        )
                    }
                    composable(Screen.VoiceAdd.route) {
                        VoiceAddScreen(
                            viewModel = viewModel,
                            onNavigateBack = { navController.popBackStack() }
                        )
                    }
                    composable(Screen.OcrCapture.route) {
                        OcrCaptureScreen(
                            viewModel = viewModel,
                            onNavigateBack = { navController.popBackStack() }
                        )
                    }
                    composable(Screen.QuickAdd.route) {
                        QuickAddScreen(
                            viewModel = viewModel,
                            onNavigateBack = { navController.popBackStack() }
                        )
                    }
                }
            }

            // Speed dial FAB overlay
            if (showFab) {
                SpeedDialFab(
                    onTypeClick = { navController.navigate(Screen.AddTask.route) },
                    onSpeakClick = { navController.navigate(Screen.VoiceAdd.route) },
                    onCameraClick = { navController.navigate(Screen.OcrCapture.route) },
                    onQuickAddClick = { navController.navigate(Screen.QuickAdd.route) }
                )
            }

            // Confetti overlay rendered above everything
            ConfettiOverlay(
                show = uiState.showConfetti,
                onFinished = viewModel::clearConfetti
            )
        }
    }
}
