// TodoDetailSheet.kt
//
// SPDX-License-Identifier: MIT
//
// Copyright (C) 2026 Anthony Green <green@moxielogic.com>

package com.cloodoo.app.ui.screens

import android.content.Intent
import android.net.Uri
import androidx.compose.foundation.clickable
import androidx.compose.foundation.gestures.detectTapGestures
import androidx.compose.foundation.gestures.rememberTransformableState
import androidx.compose.foundation.gestures.transformable
import androidx.compose.foundation.horizontalScroll
import androidx.compose.foundation.layout.*
import androidx.compose.foundation.rememberScrollState
import androidx.compose.foundation.shape.RoundedCornerShape
import androidx.compose.material.icons.Icons
import androidx.compose.material.icons.filled.Clear
import androidx.compose.material.icons.filled.Close
import androidx.compose.material.icons.filled.Edit
import androidx.compose.material.icons.filled.Share
import androidx.compose.material3.*
import androidx.compose.runtime.*
import androidx.compose.ui.Alignment
import androidx.compose.ui.Modifier
import androidx.compose.ui.draw.clip
import androidx.compose.ui.geometry.Offset
import androidx.compose.ui.graphics.graphicsLayer
import androidx.compose.ui.input.pointer.pointerInput
import androidx.compose.ui.layout.ContentScale
import androidx.compose.ui.platform.LocalContext
import androidx.compose.ui.text.style.TextOverflow
import androidx.compose.ui.unit.dp
import androidx.compose.ui.window.Dialog
import androidx.compose.ui.window.DialogProperties
import androidx.core.content.FileProvider
import coil.compose.AsyncImage
import com.cloodoo.app.data.local.TodoEntity
import com.cloodoo.app.ui.components.ClickableTextWithLinks
import com.cloodoo.app.ui.util.formatDate
import com.cloodoo.app.ui.util.millisToIsoDate
import com.cloodoo.app.ui.util.parseDateToMillis
import com.cloodoo.app.ui.util.parseTags
import com.cloodoo.app.ui.util.tagsToStorageString
import com.google.gson.Gson
import com.google.gson.reflect.TypeToken
import kotlinx.coroutines.launch
import java.io.File

@OptIn(ExperimentalMaterial3Api::class)
@Composable
fun TodoDetailSheet(
    todo: TodoEntity,
    onDismiss: () -> Unit,
    onUpdate: (todoId: String, dueDate: String?, scheduledDate: String?, tags: String?) -> Unit,
    onEdit: (todoId: String) -> Unit,
    getAttachmentPath: (suspend (String) -> String?)? = null
) {
    val context = LocalContext.current
    val sheetState = rememberModalBottomSheetState(skipPartiallyExpanded = true)

    val tags = remember(todo.tags) { parseTags(todo.tags) }
    var editableTags by remember(todo.tags) { mutableStateOf(tags) }
    var newTagText by remember { mutableStateOf("") }
    var showDueDatePicker by remember { mutableStateOf(false) }
    var showScheduledDatePicker by remember { mutableStateOf(false) }

    // Parse attachment hashes
    val attachmentHashes = remember(todo.attachmentHashes) {
        if (todo.attachmentHashes.isNullOrBlank()) {
            emptyList()
        } else {
            try {
                val type = object : TypeToken<List<String>>() {}.type
                Gson().fromJson<List<String>>(todo.attachmentHashes, type)
            } catch (e: Exception) {
                emptyList()
            }
        }
    }

    // Load attachment paths
    var attachmentPaths by remember { mutableStateOf<Map<String, String?>>(emptyMap()) }
    var selectedImagePath by remember { mutableStateOf<String?>(null) }
    val coroutineScope = rememberCoroutineScope()

    LaunchedEffect(attachmentHashes, getAttachmentPath) {
        if (getAttachmentPath != null && attachmentHashes.isNotEmpty()) {
            attachmentHashes.forEach { hash ->
                coroutineScope.launch {
                    val path = getAttachmentPath(hash)
                    attachmentPaths = attachmentPaths + (hash to path)
                }
            }
        }
    }

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
            Row(
                modifier = Modifier.fillMaxWidth(),
                horizontalArrangement = Arrangement.SpaceBetween,
                verticalAlignment = Alignment.Top
            ) {
                Text(
                    text = todo.title,
                    style = MaterialTheme.typography.headlineSmall,
                    color = MaterialTheme.colorScheme.onSurface,
                    modifier = Modifier.weight(1f)
                )
                IconButton(onClick = {
                    onDismiss()
                    onEdit(todo.id)
                }) {
                    Icon(
                        Icons.Default.Edit,
                        contentDescription = "Edit",
                        tint = MaterialTheme.colorScheme.primary
                    )
                }
            }

            Spacer(modifier = Modifier.height(16.dp))

            Row(horizontalArrangement = Arrangement.spacedBy(8.dp)) {
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

            Spacer(modifier = Modifier.height(16.dp))
            HorizontalDivider()
            Spacer(modifier = Modifier.height(16.dp))

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

            Spacer(modifier = Modifier.height(16.dp))
            HorizontalDivider()
            Spacer(modifier = Modifier.height(16.dp))

            Text(
                text = "Tags",
                style = MaterialTheme.typography.labelLarge,
                color = MaterialTheme.colorScheme.onSurfaceVariant
            )
            Spacer(modifier = Modifier.height(8.dp))

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
                                        val newTagsCsv = tagsToStorageString(editableTags)
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
                            val newTagsCsv = tagsToStorageString(editableTags)
                            onUpdate(todo.id, null, null, newTagsCsv)
                        }
                    },
                    enabled = newTagText.isNotBlank()
                ) {
                    Text("Add")
                }
            }

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
                    overflow = TextOverflow.Ellipsis,
                    modifier = Modifier.clickable {
                        try {
                            val intent = Intent(Intent.ACTION_VIEW, Uri.parse(todo.url))
                            context.startActivity(intent)
                        } catch (_: Exception) { }
                    }
                )
            }

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

            // Attachments section
            if (attachmentHashes.isNotEmpty()) {
                Spacer(modifier = Modifier.height(16.dp))
                HorizontalDivider()
                Spacer(modifier = Modifier.height(16.dp))

                Text(
                    text = "Attachments",
                    style = MaterialTheme.typography.labelLarge,
                    color = MaterialTheme.colorScheme.onSurfaceVariant
                )
                Spacer(modifier = Modifier.height(8.dp))
                Row(
                    modifier = Modifier
                        .fillMaxWidth()
                        .horizontalScroll(rememberScrollState()),
                    horizontalArrangement = Arrangement.spacedBy(8.dp)
                ) {
                    attachmentHashes.forEach { hash ->
                        val path = attachmentPaths[hash]
                        Box(
                            modifier = Modifier
                                .size(80.dp)
                                .clip(RoundedCornerShape(8.dp))
                                .then(
                                    if (path != null) Modifier.clickable {
                                        selectedImagePath = path
                                    } else Modifier
                                )
                        ) {
                            if (path != null) {
                                AsyncImage(
                                    model = Uri.fromFile(File(path)),
                                    contentDescription = "Attachment",
                                    modifier = Modifier.fillMaxSize(),
                                    contentScale = ContentScale.Crop
                                )
                            } else {
                                Surface(
                                    modifier = Modifier.fillMaxSize(),
                                    color = MaterialTheme.colorScheme.surfaceVariant
                                ) {
                                    Box(contentAlignment = Alignment.Center) {
                                        CircularProgressIndicator(
                                            modifier = Modifier.size(24.dp),
                                            strokeWidth = 2.dp
                                        )
                                    }
                                }
                            }
                        }
                    }
                }
            }
        }
    }

    // Fullscreen image viewer with zoom/pan/share
    if (selectedImagePath != null) {
        FullscreenImageViewer(
            imagePath = selectedImagePath!!,
            onDismiss = { selectedImagePath = null }
        )
    }

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
fun FullscreenImageViewer(
    imagePath: String,
    onDismiss: () -> Unit
) {
    val context = LocalContext.current

    // Zoom/pan state - use mutableFloatStateOf for better performance
    var scale by remember { mutableFloatStateOf(1f) }
    var offsetX by remember { mutableFloatStateOf(0f) }
    var offsetY by remember { mutableFloatStateOf(0f) }

    val transformableState = rememberTransformableState { zoomChange, panChange, _ ->
        val newScale = (scale * zoomChange).coerceIn(1f, 5f)

        // Only allow panning when zoomed in
        // Scale pan sensitivity by zoom level for more responsive movement
        if (newScale > 1f) {
            val panMultiplier = 1.5f // Increase pan sensitivity
            offsetX += panChange.x * panMultiplier
            offsetY += panChange.y * panMultiplier
        } else {
            offsetX = 0f
            offsetY = 0f
        }

        scale = newScale
    }

    // Reset zoom on double tap
    val doubleTapModifier = Modifier.pointerInput(Unit) {
        detectTapGestures(
            onDoubleTap = {
                if (scale > 1f) {
                    scale = 1f
                    offsetX = 0f
                    offsetY = 0f
                } else {
                    scale = 2.5f
                }
            }
        )
    }

    Dialog(
        onDismissRequest = onDismiss,
        properties = DialogProperties(
            usePlatformDefaultWidth = false,
            decorFitsSystemWindows = false
        )
    ) {
        Surface(
            modifier = Modifier.fillMaxSize(),
            color = MaterialTheme.colorScheme.scrim
        ) {
            Box(modifier = Modifier.fillMaxSize()) {
                // Image with zoom/pan
                AsyncImage(
                    model = Uri.fromFile(File(imagePath)),
                    contentDescription = "Attachment fullscreen",
                    modifier = Modifier
                        .fillMaxSize()
                        .then(doubleTapModifier)
                        .transformable(transformableState)
                        .graphicsLayer {
                            scaleX = scale
                            scaleY = scale
                            translationX = offsetX
                            translationY = offsetY
                        },
                    contentScale = ContentScale.Fit
                )

                // Top bar with close and share buttons
                Row(
                    modifier = Modifier
                        .fillMaxWidth()
                        .padding(16.dp)
                        .align(Alignment.TopCenter),
                    horizontalArrangement = Arrangement.SpaceBetween
                ) {
                    // Close button
                    IconButton(
                        onClick = onDismiss,
                        colors = IconButtonDefaults.iconButtonColors(
                            containerColor = MaterialTheme.colorScheme.surface.copy(alpha = 0.7f)
                        )
                    ) {
                        Icon(
                            Icons.Default.Close,
                            contentDescription = "Close",
                            tint = MaterialTheme.colorScheme.onSurface
                        )
                    }

                    // Share button
                    IconButton(
                        onClick = {
                            val imageFile = File(imagePath)

                            // Detect MIME type and create temp file with proper extension
                            val options = android.graphics.BitmapFactory.Options()
                            options.inJustDecodeBounds = true
                            android.graphics.BitmapFactory.decodeFile(imagePath, options)
                            val mimeType = options.outMimeType ?: "image/jpeg"
                            val extension = when (mimeType) {
                                "image/png" -> "png"
                                "image/jpeg" -> "jpg"
                                "image/gif" -> "gif"
                                "image/webp" -> "webp"
                                else -> "jpg"
                            }

                            // Copy to temp file with proper name
                            val tempDir = File(context.cacheDir, "shared")
                            tempDir.mkdirs()
                            val tempFile = File(tempDir, "photo.$extension")
                            imageFile.copyTo(tempFile, overwrite = true)

                            val imageUri = FileProvider.getUriForFile(
                                context,
                                "${context.packageName}.fileprovider",
                                tempFile
                            )

                            val shareIntent = Intent(Intent.ACTION_SEND).apply {
                                type = mimeType
                                putExtra(Intent.EXTRA_STREAM, imageUri)
                                addFlags(Intent.FLAG_GRANT_READ_URI_PERMISSION)
                            }

                            context.startActivity(Intent.createChooser(shareIntent, "Share photo"))
                        },
                        colors = IconButtonDefaults.iconButtonColors(
                            containerColor = MaterialTheme.colorScheme.surface.copy(alpha = 0.7f)
                        )
                    ) {
                        Icon(
                            Icons.Default.Share,
                            contentDescription = "Share",
                            tint = MaterialTheme.colorScheme.onSurface
                        )
                    }
                }
            }
        }
    }
}
