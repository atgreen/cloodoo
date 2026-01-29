// VoiceAddScreen.kt
//
// SPDX-License-Identifier: MIT
//
// Copyright (C) 2026 Anthony Green <green@moxielogic.com>

package com.cloodoo.app.ui.screens

import android.Manifest
import android.content.Intent
import android.os.Build
import android.os.Bundle
import android.speech.RecognitionListener
import android.speech.RecognizerIntent
import android.speech.SpeechRecognizer
import androidx.activity.compose.rememberLauncherForActivityResult
import androidx.activity.result.contract.ActivityResultContracts
import androidx.compose.animation.core.*
import androidx.compose.foundation.layout.*
import androidx.compose.foundation.rememberScrollState
import androidx.compose.foundation.verticalScroll
import androidx.compose.material.icons.Icons
import androidx.compose.material.icons.filled.Close
import androidx.compose.material.icons.filled.Mic
import androidx.compose.material3.*
import androidx.compose.runtime.*
import androidx.compose.ui.Alignment
import androidx.compose.ui.Modifier
import androidx.compose.ui.draw.scale
import androidx.compose.ui.platform.LocalContext
import androidx.compose.ui.text.style.TextAlign
import androidx.compose.ui.unit.dp

private enum class VoiceState {
    READY, LISTENING, PROCESSING, RESULT, MULTI_RESULT, ERROR, PERMISSION_DENIED
}

private fun splitIntoTasks(text: String): List<String> {
    // Try numbered items: "1. foo 2. bar"
    val numbered = Regex("""(?:^|\s)\d+[.)]\s*""").split(text).map { it.trim() }.filter { it.isNotBlank() }
    if (numbered.size > 1) return numbered

    // Try "and" separator: "foo, bar, and baz" or "foo and bar"
    val andSplit = text.split(Regex(""",\s+and\s+|\s+and\s+""", RegexOption.IGNORE_CASE))
        .flatMap { it.split(",") }
        .map { it.trim() }
        .filter { it.isNotBlank() }
    if (andSplit.size > 1) return andSplit

    return listOf(text)
}

@OptIn(ExperimentalMaterial3Api::class)
@Composable
fun VoiceAddScreen(
    viewModel: TodoListViewModel,
    onNavigateBack: () -> Unit,
) {
    val context = LocalContext.current

    var voiceState by remember { mutableStateOf(VoiceState.READY) }
    var recognizedText by remember { mutableStateOf("") }
    var errorMessage by remember { mutableStateOf("") }
    var hasPermission by remember { mutableStateOf(false) }
    var rmsLevel by remember { mutableFloatStateOf(0f) }
    var splitTasks by remember { mutableStateOf<List<String>>(emptyList()) }
    var selectedTasks by remember { mutableStateOf<Set<Int>>(emptySet()) }

    val permissionLauncher = rememberLauncherForActivityResult(
        ActivityResultContracts.RequestPermission()
    ) { granted ->
        hasPermission = granted
        if (!granted) {
            voiceState = VoiceState.PERMISSION_DENIED
        }
    }

    // Request permission on first composition
    LaunchedEffect(Unit) {
        permissionLauncher.launch(Manifest.permission.RECORD_AUDIO)
    }

    val speechRecognizer = remember {
        if (Build.VERSION.SDK_INT >= Build.VERSION_CODES.TIRAMISU &&
            SpeechRecognizer.isOnDeviceRecognitionAvailable(context)
        ) {
            SpeechRecognizer.createOnDeviceSpeechRecognizer(context)
        } else {
            SpeechRecognizer.createSpeechRecognizer(context)
        }
    }

    DisposableEffect(Unit) {
        onDispose {
            speechRecognizer.destroy()
        }
    }

    val recognitionListener = remember {
        object : RecognitionListener {
            override fun onReadyForSpeech(params: Bundle?) {
                voiceState = VoiceState.LISTENING
            }

            override fun onBeginningOfSpeech() {}

            override fun onRmsChanged(rmsdB: Float) {
                rmsLevel = rmsdB.coerceIn(0f, 12f) / 12f
            }

            override fun onBufferReceived(buffer: ByteArray?) {}

            override fun onEndOfSpeech() {
                voiceState = VoiceState.PROCESSING
            }

            override fun onError(error: Int) {
                errorMessage = when (error) {
                    SpeechRecognizer.ERROR_NO_MATCH -> "No speech detected. Please try again."
                    SpeechRecognizer.ERROR_SPEECH_TIMEOUT -> "No speech heard. Please try again."
                    SpeechRecognizer.ERROR_AUDIO -> "Audio recording error."
                    SpeechRecognizer.ERROR_NETWORK -> "Network error. On-device recognition may not be available."
                    SpeechRecognizer.ERROR_NETWORK_TIMEOUT -> "Network timeout."
                    SpeechRecognizer.ERROR_INSUFFICIENT_PERMISSIONS -> "Microphone permission not granted."
                    else -> "Recognition error (code $error). Please try again."
                }
                voiceState = VoiceState.ERROR
            }

            override fun onResults(results: Bundle?) {
                val matches = results?.getStringArrayList(SpeechRecognizer.RESULTS_RECOGNITION)
                if (!matches.isNullOrEmpty()) {
                    recognizedText = matches[0]
                    val tasks = splitIntoTasks(recognizedText)
                    if (tasks.size > 1) {
                        splitTasks = tasks
                        selectedTasks = tasks.indices.toSet()
                        voiceState = VoiceState.MULTI_RESULT
                    } else {
                        voiceState = VoiceState.RESULT
                    }
                } else {
                    errorMessage = "No speech detected. Please try again."
                    voiceState = VoiceState.ERROR
                }
            }

            override fun onPartialResults(partialResults: Bundle?) {
                val partial = partialResults?.getStringArrayList(SpeechRecognizer.RESULTS_RECOGNITION)
                if (!partial.isNullOrEmpty()) {
                    recognizedText = partial[0]
                }
            }

            override fun onEvent(eventType: Int, params: Bundle?) {}
        }
    }

    fun startListening() {
        recognizedText = ""
        rmsLevel = 0f
        voiceState = VoiceState.READY
        val intent = Intent(RecognizerIntent.ACTION_RECOGNIZE_SPEECH).apply {
            putExtra(RecognizerIntent.EXTRA_LANGUAGE_MODEL, RecognizerIntent.LANGUAGE_MODEL_FREE_FORM)
            putExtra(RecognizerIntent.EXTRA_PARTIAL_RESULTS, true)
            putExtra(RecognizerIntent.EXTRA_MAX_RESULTS, 1)
        }
        speechRecognizer.setRecognitionListener(recognitionListener)
        speechRecognizer.startListening(intent)
    }

    Scaffold(
        topBar = {
            TopAppBar(
                title = { Text("Voice Add") },
                navigationIcon = {
                    IconButton(onClick = {
                        speechRecognizer.cancel()
                        onNavigateBack()
                    }) {
                        Icon(Icons.Default.Close, contentDescription = "Close")
                    }
                }
            )
        }
    ) { padding ->
        Column(
            modifier = Modifier
                .fillMaxSize()
                .padding(padding)
                .padding(24.dp),
            horizontalAlignment = Alignment.CenterHorizontally,
            verticalArrangement = Arrangement.Center
        ) {
            when (voiceState) {
                VoiceState.PERMISSION_DENIED -> {
                    Text(
                        text = "Microphone permission is required for voice input.",
                        style = MaterialTheme.typography.bodyLarge,
                        textAlign = TextAlign.Center
                    )
                    Spacer(modifier = Modifier.height(16.dp))
                    Button(onClick = {
                        permissionLauncher.launch(Manifest.permission.RECORD_AUDIO)
                    }) {
                        Text("Grant Permission")
                    }
                }

                VoiceState.READY -> {
                    if (hasPermission) {
                        MicButton(
                            pulsing = false,
                            rmsLevel = 0f,
                            onClick = { startListening() }
                        )
                        Spacer(modifier = Modifier.height(16.dp))
                        Text(
                            text = "Tap to speak",
                            style = MaterialTheme.typography.bodyLarge,
                            color = MaterialTheme.colorScheme.onSurfaceVariant
                        )
                    }
                }

                VoiceState.LISTENING -> {
                    MicButton(
                        pulsing = true,
                        rmsLevel = rmsLevel,
                        onClick = { speechRecognizer.stopListening() }
                    )
                    Spacer(modifier = Modifier.height(16.dp))
                    Text(
                        text = "Listening...",
                        style = MaterialTheme.typography.bodyLarge,
                        color = MaterialTheme.colorScheme.primary
                    )
                    if (recognizedText.isNotEmpty()) {
                        Spacer(modifier = Modifier.height(12.dp))
                        Text(
                            text = recognizedText,
                            style = MaterialTheme.typography.bodyMedium,
                            color = MaterialTheme.colorScheme.onSurfaceVariant,
                            textAlign = TextAlign.Center
                        )
                    }
                }

                VoiceState.PROCESSING -> {
                    CircularProgressIndicator()
                    Spacer(modifier = Modifier.height(16.dp))
                    Text(
                        text = "Processing...",
                        style = MaterialTheme.typography.bodyLarge,
                        color = MaterialTheme.colorScheme.onSurfaceVariant
                    )
                }

                VoiceState.RESULT -> {
                    Text(
                        text = recognizedText,
                        style = MaterialTheme.typography.headlineSmall,
                        textAlign = TextAlign.Center,
                        modifier = Modifier.padding(horizontal = 16.dp)
                    )
                    Spacer(modifier = Modifier.height(32.dp))
                    Row(horizontalArrangement = Arrangement.spacedBy(16.dp)) {
                        OutlinedButton(onClick = { startListening() }) {
                            Text("Try Again")
                        }
                        Button(onClick = {
                            viewModel.createTodo(title = recognizedText)
                            onNavigateBack()
                        }) {
                            Text("Save")
                        }
                    }
                }

                VoiceState.MULTI_RESULT -> {
                    Column(
                        modifier = Modifier
                            .verticalScroll(rememberScrollState())
                            .padding(horizontal = 16.dp),
                        verticalArrangement = Arrangement.spacedBy(8.dp)
                    ) {
                        Text(
                            text = "${splitTasks.size} tasks detected",
                            style = MaterialTheme.typography.titleMedium
                        )
                        Spacer(modifier = Modifier.height(8.dp))
                        splitTasks.forEachIndexed { index, task ->
                            Row(
                                verticalAlignment = Alignment.CenterVertically,
                                modifier = Modifier.fillMaxWidth()
                            ) {
                                Checkbox(
                                    checked = index in selectedTasks,
                                    onCheckedChange = { checked ->
                                        selectedTasks = if (checked) {
                                            selectedTasks + index
                                        } else {
                                            selectedTasks - index
                                        }
                                    }
                                )
                                Text(
                                    text = task,
                                    style = MaterialTheme.typography.bodyLarge,
                                    modifier = Modifier.weight(1f)
                                )
                            }
                        }
                        Spacer(modifier = Modifier.height(16.dp))
                        Row(
                            horizontalArrangement = Arrangement.spacedBy(16.dp),
                            modifier = Modifier.fillMaxWidth()
                        ) {
                            OutlinedButton(
                                onClick = { startListening() },
                                modifier = Modifier.weight(1f)
                            ) {
                                Text("Try Again")
                            }
                            Button(
                                onClick = {
                                    selectedTasks.sorted().forEach { index ->
                                        viewModel.createTodo(title = splitTasks[index])
                                    }
                                    onNavigateBack()
                                },
                                enabled = selectedTasks.isNotEmpty(),
                                modifier = Modifier.weight(1f)
                            ) {
                                Text("Save ${selectedTasks.size} Tasks")
                            }
                        }
                    }
                }

                VoiceState.ERROR -> {
                    Text(
                        text = errorMessage,
                        style = MaterialTheme.typography.bodyLarge,
                        textAlign = TextAlign.Center,
                        color = MaterialTheme.colorScheme.error
                    )
                    Spacer(modifier = Modifier.height(16.dp))
                    Button(onClick = { startListening() }) {
                        Text("Try Again")
                    }
                }
            }
        }
    }
}

@Composable
private fun MicButton(
    pulsing: Boolean,
    rmsLevel: Float,
    onClick: () -> Unit,
) {
    val infiniteTransition = rememberInfiniteTransition(label = "mic_pulse")
    val pulseScale by infiniteTransition.animateFloat(
        initialValue = 1f,
        targetValue = 1.15f,
        animationSpec = infiniteRepeatable(
            animation = tween(600, easing = EaseInOut),
            repeatMode = RepeatMode.Reverse
        ),
        label = "pulse_scale"
    )

    val scale = if (pulsing) pulseScale + (rmsLevel * 0.15f) else 1f

    LargeFloatingActionButton(
        onClick = onClick,
        modifier = Modifier.scale(scale),
        containerColor = if (pulsing)
            MaterialTheme.colorScheme.error
        else
            MaterialTheme.colorScheme.primaryContainer,
        contentColor = if (pulsing)
            MaterialTheme.colorScheme.onError
        else
            MaterialTheme.colorScheme.onPrimaryContainer
    ) {
        Icon(
            Icons.Default.Mic,
            contentDescription = if (pulsing) "Stop listening" else "Start listening",
            modifier = Modifier.size(36.dp)
        )
    }
}
