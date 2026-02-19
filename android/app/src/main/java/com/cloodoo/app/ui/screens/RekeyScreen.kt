// RekeyScreen.kt
//
// SPDX-License-Identifier: MIT
//
// Copyright (C) 2026 Anthony Green <green@moxielogic.com>

package com.cloodoo.app.ui.screens

import android.graphics.Bitmap
import androidx.compose.foundation.Image
import androidx.compose.foundation.layout.*
import androidx.compose.material.icons.Icons
import androidx.compose.material.icons.automirrored.filled.ArrowBack
import androidx.compose.material3.*
import androidx.compose.runtime.*
import androidx.compose.ui.Alignment
import androidx.compose.ui.Modifier
import androidx.compose.ui.graphics.asImageBitmap
import androidx.compose.ui.text.font.FontFamily
import androidx.compose.ui.text.style.TextAlign
import androidx.compose.ui.unit.dp
import com.cloodoo.app.data.remote.GrpcSyncClient
import com.google.zxing.BarcodeFormat
import com.google.zxing.qrcode.QRCodeWriter
import kotlinx.coroutines.Dispatchers
import kotlinx.coroutines.delay
import kotlinx.coroutines.launch
import kotlinx.coroutines.withContext

@OptIn(ExperimentalMaterial3Api::class)
@Composable
fun RekeyScreen(
    grpcClient: GrpcSyncClient,
    onBack: () -> Unit
) {
    var deviceName by remember { mutableStateOf("") }
    var pairingUrl by remember { mutableStateOf<String?>(null) }
    var passphrase by remember { mutableStateOf<String?>(null) }
    var expiresIn by remember { mutableIntStateOf(0) }
    var errorMessage by remember { mutableStateOf<String?>(null) }
    var isLoading by remember { mutableStateOf(false) }
    val scope = rememberCoroutineScope()

    // Countdown timer
    LaunchedEffect(expiresIn) {
        if (expiresIn > 0) {
            delay(1000)
            expiresIn -= 1
        }
    }

    Scaffold(
        topBar = {
            TopAppBar(
                title = { Text("Pair New Device") },
                navigationIcon = {
                    IconButton(onClick = onBack) {
                        Icon(Icons.AutoMirrored.Filled.ArrowBack, "Back")
                    }
                }
            )
        }
    ) { padding ->
        Column(
            modifier = Modifier
                .padding(padding)
                .padding(16.dp)
                .fillMaxSize(),
            horizontalAlignment = Alignment.CenterHorizontally
        ) {
            if (pairingUrl == null) {
                // Input form
                Text(
                    "Enter a name for the new device, then share the QR code with it.",
                    style = MaterialTheme.typography.bodyLarge,
                    modifier = Modifier.padding(bottom = 24.dp)
                )

                OutlinedTextField(
                    value = deviceName,
                    onValueChange = { deviceName = it },
                    label = { Text("Device name") },
                    singleLine = true,
                    modifier = Modifier.fillMaxWidth()
                )

                Spacer(modifier = Modifier.height(16.dp))

                Button(
                    onClick = {
                        if (deviceName.isNotBlank()) {
                            isLoading = true
                            errorMessage = null
                            scope.launch {
                                try {
                                    val response = withContext(Dispatchers.IO) {
                                        grpcClient.requestRekey(deviceName.trim())
                                    }
                                    if (response.error.isNotEmpty()) {
                                        errorMessage = response.error
                                    } else {
                                        pairingUrl = response.pairingUrl
                                        passphrase = response.passphrase
                                        expiresIn = response.expiresIn
                                    }
                                } catch (e: Exception) {
                                    errorMessage = e.message ?: "Failed to create pairing request"
                                } finally {
                                    isLoading = false
                                }
                            }
                        }
                    },
                    enabled = deviceName.isNotBlank() && !isLoading,
                    modifier = Modifier.fillMaxWidth()
                ) {
                    if (isLoading) {
                        CircularProgressIndicator(
                            modifier = Modifier.size(20.dp),
                            strokeWidth = 2.dp,
                            color = MaterialTheme.colorScheme.onPrimary
                        )
                        Spacer(modifier = Modifier.width(8.dp))
                    }
                    Text("Generate Pairing Code")
                }

                errorMessage?.let { error ->
                    Spacer(modifier = Modifier.height(16.dp))
                    Card(
                        colors = CardDefaults.cardColors(
                            containerColor = MaterialTheme.colorScheme.errorContainer
                        )
                    ) {
                        Text(
                            text = error,
                            color = MaterialTheme.colorScheme.onErrorContainer,
                            modifier = Modifier.padding(16.dp)
                        )
                    }
                }
            } else {
                // QR code display
                val qrBitmap = remember(pairingUrl) {
                    generateQrBitmap(pairingUrl!!, 512)
                }

                if (qrBitmap != null) {
                    Image(
                        bitmap = qrBitmap.asImageBitmap(),
                        contentDescription = "Pairing QR Code",
                        modifier = Modifier
                            .size(256.dp)
                            .padding(8.dp)
                    )
                }

                Spacer(modifier = Modifier.height(16.dp))

                Text(
                    "Scan this QR code with the Cloodoo app on the new device",
                    style = MaterialTheme.typography.bodyLarge,
                    textAlign = TextAlign.Center
                )

                Spacer(modifier = Modifier.height(16.dp))

                Text("Passphrase:", style = MaterialTheme.typography.labelLarge)
                Card(
                    colors = CardDefaults.cardColors(
                        containerColor = MaterialTheme.colorScheme.primaryContainer
                    ),
                    modifier = Modifier.padding(vertical = 8.dp)
                ) {
                    Text(
                        text = passphrase ?: "",
                        fontFamily = FontFamily.Monospace,
                        style = MaterialTheme.typography.headlineSmall,
                        color = MaterialTheme.colorScheme.onPrimaryContainer,
                        modifier = Modifier.padding(16.dp)
                    )
                }

                Spacer(modifier = Modifier.height(16.dp))

                if (expiresIn > 0) {
                    val minutes = expiresIn / 60
                    val seconds = expiresIn % 60
                    Text(
                        "Expires in ${minutes}m ${seconds}s",
                        style = MaterialTheme.typography.bodyMedium,
                        color = if (expiresIn < 60)
                            MaterialTheme.colorScheme.error
                        else
                            MaterialTheme.colorScheme.onSurfaceVariant
                    )
                } else if (pairingUrl != null) {
                    Text(
                        "Expired",
                        style = MaterialTheme.typography.bodyMedium,
                        color = MaterialTheme.colorScheme.error
                    )
                }
            }
        }
    }
}

private fun generateQrBitmap(data: String, size: Int): Bitmap? {
    return try {
        val writer = QRCodeWriter()
        val bitMatrix = writer.encode(data, BarcodeFormat.QR_CODE, size, size)
        val bitmap = Bitmap.createBitmap(size, size, Bitmap.Config.RGB_565)
        for (x in 0 until size) {
            for (y in 0 until size) {
                bitmap.setPixel(x, y, if (bitMatrix[x, y]) 0xFF000000.toInt() else 0xFFFFFFFF.toInt())
            }
        }
        bitmap
    } catch (e: Exception) {
        null
    }
}
