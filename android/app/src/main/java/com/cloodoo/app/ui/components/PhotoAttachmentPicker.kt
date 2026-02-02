// PhotoAttachmentPicker.kt
//
// SPDX-License-Identifier: MIT
//
// Copyright (C) 2026 Anthony Green <green@moxielogic.com>

package com.cloodoo.app.ui.components

import android.Manifest
import android.net.Uri
import android.util.Log
import androidx.activity.compose.rememberLauncherForActivityResult
import androidx.activity.result.PickVisualMediaRequest
import androidx.activity.result.contract.ActivityResultContracts
import androidx.camera.core.CameraSelector
import androidx.camera.core.ImageCapture
import androidx.camera.core.ImageCaptureException
import androidx.camera.core.Preview
import androidx.camera.lifecycle.ProcessCameraProvider
import androidx.camera.view.PreviewView
import androidx.compose.foundation.background
import androidx.compose.foundation.clickable
import androidx.compose.foundation.horizontalScroll
import androidx.compose.foundation.layout.*
import androidx.compose.foundation.rememberScrollState
import androidx.compose.foundation.shape.CircleShape
import androidx.compose.foundation.shape.RoundedCornerShape
import androidx.compose.material.icons.Icons
import androidx.compose.material.icons.filled.Add
import androidx.compose.material.icons.filled.CameraAlt
import androidx.compose.material.icons.filled.Close
import androidx.compose.material.icons.filled.PhotoLibrary
import androidx.compose.material3.*
import androidx.compose.runtime.*
import androidx.compose.ui.Alignment
import androidx.compose.ui.Modifier
import androidx.compose.ui.draw.clip
import androidx.compose.ui.layout.ContentScale
import androidx.compose.ui.platform.LocalContext
import androidx.compose.ui.platform.LocalLifecycleOwner
import androidx.compose.ui.unit.dp
import androidx.compose.ui.viewinterop.AndroidView
import androidx.compose.ui.window.Dialog
import androidx.core.content.ContextCompat
import coil.compose.AsyncImage
import java.io.File

/**
 * Represents a locally-stored attachment (file copied to app storage).
 */
data class LocalAttachment(
    val uri: Uri,           // Local file URI in app storage
    val hash: String,
    val mimeType: String,
    val size: Long,
    val filename: String,
    val localPath: String   // Path to file in app storage
)

/**
 * A composable for picking and displaying photo attachments.
 * Supports camera capture and gallery selection.
 */
@OptIn(ExperimentalMaterial3Api::class)
@Composable
fun PhotoAttachmentPicker(
    attachments: List<LocalAttachment>,
    onAttachmentsChanged: (List<LocalAttachment>) -> Unit,
    modifier: Modifier = Modifier
) {
    val context = LocalContext.current
    var showBottomSheet by remember { mutableStateOf(false) }
    var showCamera by remember { mutableStateOf(false) }
    var hasCameraPermission by remember { mutableStateOf(false) }

    // Camera permission launcher
    val cameraPermissionLauncher = rememberLauncherForActivityResult(
        ActivityResultContracts.RequestPermission()
    ) { granted ->
        hasCameraPermission = granted
        if (granted) {
            showCamera = true
        }
    }

    // Attachments directory in app storage
    val attachmentsDir = remember {
        File(context.filesDir, "attachments").also { it.mkdirs() }
    }

    // Photo picker launcher
    val photoPickerLauncher = rememberLauncherForActivityResult(
        ActivityResultContracts.PickVisualMedia()
    ) { uri ->
        if (uri != null) {
            // Get file info
            val mimeType = context.contentResolver.getType(uri) ?: "image/jpeg"
            val cursor = context.contentResolver.query(uri, null, null, null, null)
            var filename = "photo.jpg"
            cursor?.use {
                if (it.moveToFirst()) {
                    val nameIndex = it.getColumnIndex(android.provider.OpenableColumns.DISPLAY_NAME)
                    if (nameIndex >= 0) filename = it.getString(nameIndex) ?: filename
                }
            }

            // Read content, compute hash, and store immediately
            val content = context.contentResolver.openInputStream(uri)?.use { it.readBytes() }
                ?: return@rememberLauncherForActivityResult

            val digest = java.security.MessageDigest.getInstance("SHA-256")
            val hash = digest.digest(content).joinToString("") { "%02x".format(it) }

            // Check for duplicates
            if (attachments.none { it.hash == hash }) {
                // Store in app storage immediately (before URI permission expires)
                val localFile = File(attachmentsDir, hash)
                localFile.writeBytes(content)
                val localPath = localFile.absolutePath

                val attachment = LocalAttachment(
                    uri = Uri.fromFile(localFile),
                    hash = hash,
                    mimeType = mimeType,
                    size = content.size.toLong(),
                    filename = filename,
                    localPath = localPath
                )
                onAttachmentsChanged(attachments + attachment)
            }
        }
    }

    Column(modifier = modifier) {
        // Thumbnail row with attachments
        if (attachments.isNotEmpty()) {
            Row(
                modifier = Modifier
                    .fillMaxWidth()
                    .horizontalScroll(rememberScrollState())
                    .padding(vertical = 8.dp),
                horizontalArrangement = Arrangement.spacedBy(8.dp)
            ) {
                attachments.forEach { attachment ->
                    Box(
                        modifier = Modifier
                            .size(72.dp)
                            .clip(RoundedCornerShape(8.dp))
                    ) {
                        AsyncImage(
                            model = attachment.uri,
                            contentDescription = attachment.filename,
                            modifier = Modifier.fillMaxSize(),
                            contentScale = ContentScale.Crop
                        )
                        // Remove button
                        IconButton(
                            onClick = {
                                onAttachmentsChanged(attachments.filter { it.hash != attachment.hash })
                            },
                            modifier = Modifier
                                .align(Alignment.TopEnd)
                                .size(24.dp)
                                .background(
                                    MaterialTheme.colorScheme.surface.copy(alpha = 0.7f),
                                    CircleShape
                                )
                        ) {
                            Icon(
                                Icons.Default.Close,
                                contentDescription = "Remove",
                                modifier = Modifier.size(16.dp)
                            )
                        }
                    }
                }
            }
        }

        // Add attachment button
        OutlinedButton(
            onClick = { showBottomSheet = true },
            modifier = Modifier.fillMaxWidth()
        ) {
            Icon(
                Icons.Default.Add,
                contentDescription = null,
                modifier = Modifier.size(18.dp)
            )
            Spacer(Modifier.width(8.dp))
            Text(if (attachments.isEmpty()) "Add Photo" else "Add Another")
        }
    }

    // Bottom sheet for camera/gallery choice
    if (showBottomSheet) {
        ModalBottomSheet(
            onDismissRequest = { showBottomSheet = false }
        ) {
            Column(
                modifier = Modifier
                    .fillMaxWidth()
                    .padding(16.dp)
            ) {
                Text(
                    "Add Photo",
                    style = MaterialTheme.typography.titleMedium,
                    modifier = Modifier.padding(bottom = 16.dp)
                )

                ListItem(
                    headlineContent = { Text("Take Photo") },
                    leadingContent = {
                        Icon(Icons.Default.CameraAlt, contentDescription = null)
                    },
                    modifier = Modifier.clickable {
                        showBottomSheet = false
                        cameraPermissionLauncher.launch(Manifest.permission.CAMERA)
                    }
                )

                ListItem(
                    headlineContent = { Text("Choose from Gallery") },
                    leadingContent = {
                        Icon(Icons.Default.PhotoLibrary, contentDescription = null)
                    },
                    modifier = Modifier.clickable {
                        showBottomSheet = false
                        photoPickerLauncher.launch(
                            PickVisualMediaRequest(ActivityResultContracts.PickVisualMedia.ImageOnly)
                        )
                    }
                )

                Spacer(Modifier.height(32.dp))
            }
        }
    }

    // Camera dialog
    if (showCamera) {
        CameraCaptureDialog(
            attachmentsDir = attachmentsDir,
            onDismiss = { showCamera = false },
            onPhotoTaken = { localFile, hash ->
                val attachment = LocalAttachment(
                    uri = Uri.fromFile(localFile),
                    hash = hash,
                    mimeType = "image/jpeg",
                    size = localFile.length(),
                    filename = "photo_${System.currentTimeMillis()}.jpg",
                    localPath = localFile.absolutePath
                )
                if (attachments.none { it.hash == hash }) {
                    onAttachmentsChanged(attachments + attachment)
                }
                showCamera = false
            }
        )
    }
}

@Composable
private fun CameraCaptureDialog(
    attachmentsDir: File,
    onDismiss: () -> Unit,
    onPhotoTaken: (File, String) -> Unit
) {
    val context = LocalContext.current
    val lifecycleOwner = LocalLifecycleOwner.current
    val imageCapture = remember { ImageCapture.Builder().build() }

    Dialog(onDismissRequest = onDismiss) {
        Surface(
            modifier = Modifier
                .fillMaxWidth()
                .aspectRatio(3f / 4f),
            shape = RoundedCornerShape(16.dp)
        ) {
            Box(modifier = Modifier.fillMaxSize()) {
                AndroidView(
                    factory = { ctx ->
                        PreviewView(ctx).also { previewView ->
                            val cameraProviderFuture = ProcessCameraProvider.getInstance(ctx)
                            cameraProviderFuture.addListener({
                                val cameraProvider = cameraProviderFuture.get()
                                val preview = Preview.Builder().build().also {
                                    it.setSurfaceProvider(previewView.surfaceProvider)
                                }
                                try {
                                    cameraProvider.unbindAll()
                                    cameraProvider.bindToLifecycle(
                                        lifecycleOwner,
                                        CameraSelector.DEFAULT_BACK_CAMERA,
                                        preview,
                                        imageCapture
                                    )
                                } catch (e: Exception) {
                                    Log.e("PhotoPicker", "Camera bind failed", e)
                                }
                            }, ContextCompat.getMainExecutor(ctx))
                        }
                    },
                    modifier = Modifier.fillMaxSize()
                )

                // Close button
                IconButton(
                    onClick = onDismiss,
                    modifier = Modifier
                        .align(Alignment.TopEnd)
                        .padding(8.dp)
                        .background(
                            MaterialTheme.colorScheme.surface.copy(alpha = 0.7f),
                            CircleShape
                        )
                ) {
                    Icon(Icons.Default.Close, contentDescription = "Close")
                }

                // Capture button
                FloatingActionButton(
                    onClick = {
                        val tempFile = File.createTempFile(
                            "photo_",
                            ".jpg",
                            context.cacheDir
                        )
                        val outputOptions = ImageCapture.OutputFileOptions.Builder(tempFile).build()
                        imageCapture.takePicture(
                            outputOptions,
                            ContextCompat.getMainExecutor(context),
                            object : ImageCapture.OnImageSavedCallback {
                                override fun onImageSaved(output: ImageCapture.OutputFileResults) {
                                    // Read content and compute hash
                                    val content = tempFile.readBytes()
                                    val digest = java.security.MessageDigest.getInstance("SHA-256")
                                    val hash = digest.digest(content).joinToString("") { "%02x".format(it) }

                                    // Move to attachments directory with hash as filename
                                    val localFile = File(attachmentsDir, hash)
                                    localFile.writeBytes(content)
                                    tempFile.delete()

                                    onPhotoTaken(localFile, hash)
                                }

                                override fun onError(exception: ImageCaptureException) {
                                    Log.e("PhotoPicker", "Capture failed", exception)
                                }
                            }
                        )
                    },
                    modifier = Modifier
                        .align(Alignment.BottomCenter)
                        .padding(24.dp),
                    containerColor = MaterialTheme.colorScheme.primary
                ) {
                    Icon(
                        Icons.Default.CameraAlt,
                        contentDescription = "Take Photo",
                        modifier = Modifier.size(32.dp)
                    )
                }
            }
        }
    }
}
