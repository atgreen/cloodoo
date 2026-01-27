package com.cloodoo.app.ui.screens

import android.Manifest
import android.util.Log
import androidx.activity.compose.rememberLauncherForActivityResult
import androidx.activity.result.contract.ActivityResultContracts
import androidx.camera.core.CameraSelector
import androidx.camera.core.ImageAnalysis
import androidx.camera.core.Preview
import androidx.camera.lifecycle.ProcessCameraProvider
import androidx.camera.view.PreviewView
import androidx.compose.foundation.layout.*
import androidx.compose.foundation.text.KeyboardOptions
import androidx.compose.material.icons.Icons
import androidx.compose.material.icons.filled.*
import androidx.compose.material3.*
import androidx.compose.runtime.*
import androidx.compose.ui.Alignment
import androidx.compose.ui.Modifier
import androidx.compose.ui.platform.LocalContext
import androidx.compose.ui.platform.LocalLifecycleOwner
import androidx.compose.ui.text.input.KeyboardType
import androidx.compose.ui.text.input.PasswordVisualTransformation
import androidx.compose.ui.unit.dp
import androidx.compose.ui.viewinterop.AndroidView
import androidx.core.content.ContextCompat
import com.google.mlkit.vision.barcode.BarcodeScanning
import com.google.mlkit.vision.barcode.common.Barcode
import com.google.mlkit.vision.common.InputImage
import kotlinx.coroutines.launch
import java.util.concurrent.Executors

/**
 * Screen for pairing with a Cloodoo server using QR code or manual entry.
 */
@OptIn(ExperimentalMaterial3Api::class)
@Composable
fun PairingScreen(
    onPairingComplete: (serverAddress: String, port: Int, certPem: String, keyPem: String, caCertPem: String?, deviceName: String) -> Unit,
    onCancel: () -> Unit
) {
    var mode by remember { mutableStateOf(PairingMode.MANUAL) }
    var scannedUrl by remember { mutableStateOf<String?>(null) }
    var manualUrl by remember { mutableStateOf("") }
    var passphrase by remember { mutableStateOf("") }
    var isLoading by remember { mutableStateOf(false) }
    var error by remember { mutableStateOf<String?>(null) }
    var hasCameraPermission by remember { mutableStateOf(false) }

    val context = LocalContext.current
    val scope = rememberCoroutineScope()

    // Camera permission launcher
    val permissionLauncher = rememberLauncherForActivityResult(
        ActivityResultContracts.RequestPermission()
    ) { granted ->
        hasCameraPermission = granted
    }

    LaunchedEffect(Unit) {
        permissionLauncher.launch(Manifest.permission.CAMERA)
    }

    Scaffold(
        topBar = {
            TopAppBar(
                title = { Text("Pair with Server") },
                navigationIcon = {
                    IconButton(onClick = onCancel) {
                        Icon(Icons.Default.Close, contentDescription = "Cancel")
                    }
                }
            )
        }
    ) { padding ->
        Column(
            modifier = Modifier
                .fillMaxSize()
                .padding(padding)
                .padding(16.dp),
            horizontalAlignment = Alignment.CenterHorizontally
        ) {
            // Mode selector
            Row(
                modifier = Modifier.fillMaxWidth(),
                horizontalArrangement = Arrangement.Center
            ) {
                FilterChip(
                    selected = mode == PairingMode.QR_SCAN,
                    onClick = { mode = PairingMode.QR_SCAN },
                    label = { Text("Scan QR") },
                    leadingIcon = {
                        Icon(Icons.Default.QrCodeScanner, contentDescription = null)
                    }
                )
                Spacer(modifier = Modifier.width(8.dp))
                FilterChip(
                    selected = mode == PairingMode.MANUAL,
                    onClick = { mode = PairingMode.MANUAL },
                    label = { Text("Manual") },
                    leadingIcon = {
                        Icon(Icons.Default.Edit, contentDescription = null)
                    }
                )
            }

            Spacer(modifier = Modifier.height(16.dp))

            when (mode) {
                PairingMode.QR_SCAN -> {
                    if (hasCameraPermission) {
                        if (scannedUrl == null) {
                            // Show QR scanner
                            QrScannerView(
                                modifier = Modifier
                                    .fillMaxWidth()
                                    .height(300.dp),
                                onQrScanned = { url ->
                                    scannedUrl = url
                                }
                            )
                            Spacer(modifier = Modifier.height(8.dp))
                            Text(
                                "Point camera at the QR code shown by 'cloodoo cert issue'",
                                style = MaterialTheme.typography.bodyMedium
                            )
                        } else {
                            // QR scanned, show URL and passphrase entry
                            Card(
                                modifier = Modifier.fillMaxWidth()
                            ) {
                                Column(modifier = Modifier.padding(16.dp)) {
                                    Row(
                                        verticalAlignment = Alignment.CenterVertically,
                                        modifier = Modifier.fillMaxWidth(),
                                        horizontalArrangement = Arrangement.SpaceBetween
                                    ) {
                                        Row(verticalAlignment = Alignment.CenterVertically) {
                                            Icon(
                                                Icons.Default.CheckCircle,
                                                contentDescription = null,
                                                tint = MaterialTheme.colorScheme.primary
                                            )
                                            Spacer(modifier = Modifier.width(8.dp))
                                            Text("QR Code Scanned")
                                        }
                                        TextButton(onClick = {
                                            scannedUrl = null
                                            error = null
                                        }) {
                                            Text("Scan Again")
                                        }
                                    }
                                    Spacer(modifier = Modifier.height(8.dp))
                                    Text(
                                        scannedUrl!!,
                                        style = MaterialTheme.typography.bodySmall
                                    )
                                }
                            }

                            Spacer(modifier = Modifier.height(16.dp))

                            OutlinedTextField(
                                value = passphrase,
                                onValueChange = { passphrase = it },
                                label = { Text("Passphrase") },
                                placeholder = { Text("tiger-blue-forest") },
                                modifier = Modifier.fillMaxWidth(),
                                singleLine = true,
                                visualTransformation = PasswordVisualTransformation()
                            )

                            Spacer(modifier = Modifier.height(8.dp))

                            Text(
                                "Enter the passphrase shown on the server",
                                style = MaterialTheme.typography.bodySmall
                            )
                        }
                    } else {
                        // No camera permission
                        Column(
                            horizontalAlignment = Alignment.CenterHorizontally,
                            modifier = Modifier.padding(32.dp)
                        ) {
                            Icon(
                                Icons.Default.CameraAlt,
                                contentDescription = null,
                                modifier = Modifier.size(64.dp),
                                tint = MaterialTheme.colorScheme.onSurfaceVariant
                            )
                            Spacer(modifier = Modifier.height(16.dp))
                            Text("Camera permission required for QR scanning")
                            Spacer(modifier = Modifier.height(8.dp))
                            Button(onClick = {
                                permissionLauncher.launch(Manifest.permission.CAMERA)
                            }) {
                                Text("Grant Permission")
                            }
                        }
                    }
                }

                PairingMode.MANUAL -> {
                    OutlinedTextField(
                        value = manualUrl,
                        onValueChange = { manualUrl = it },
                        label = { Text("Pairing URL") },
                        placeholder = { Text("http://192.168.1.100:9876/pair/abc123") },
                        modifier = Modifier.fillMaxWidth(),
                        singleLine = true,
                        keyboardOptions = KeyboardOptions(keyboardType = KeyboardType.Uri)
                    )

                    Spacer(modifier = Modifier.height(16.dp))

                    OutlinedTextField(
                        value = passphrase,
                        onValueChange = { passphrase = it },
                        label = { Text("Passphrase") },
                        placeholder = { Text("tiger-blue-forest") },
                        modifier = Modifier.fillMaxWidth(),
                        singleLine = true,
                        visualTransformation = PasswordVisualTransformation()
                    )

                    Spacer(modifier = Modifier.height(8.dp))

                    Text(
                        "Enter the URL and passphrase shown by 'cloodoo cert issue'",
                        style = MaterialTheme.typography.bodySmall
                    )
                }
            }

            Spacer(modifier = Modifier.height(24.dp))

            // Error display
            error?.let {
                Card(
                    colors = CardDefaults.cardColors(
                        containerColor = MaterialTheme.colorScheme.errorContainer
                    ),
                    modifier = Modifier.fillMaxWidth()
                ) {
                    Row(
                        modifier = Modifier.padding(16.dp),
                        verticalAlignment = Alignment.CenterVertically
                    ) {
                        Icon(
                            Icons.Default.Error,
                            contentDescription = null,
                            tint = MaterialTheme.colorScheme.error
                        )
                        Spacer(modifier = Modifier.width(8.dp))
                        Text(it, color = MaterialTheme.colorScheme.error)
                    }
                }
                Spacer(modifier = Modifier.height(16.dp))
            }

            Spacer(modifier = Modifier.weight(1f))

            // Action button
            Button(
                onClick = {
                    val url = if (mode == PairingMode.QR_SCAN) scannedUrl else manualUrl
                    if (url.isNullOrBlank()) {
                        error = "Please scan a QR code or enter a URL"
                        return@Button
                    }
                    if (passphrase.isBlank()) {
                        error = "Please enter the passphrase"
                        return@Button
                    }

                    // Reject http:// URLs to non-private IPs.
                    // Pairing is a short-lived LAN operation so private/Tailscale IPs are OK over HTTP.
                    try {
                        val parsed = java.net.URL(url)
                        if (parsed.protocol.equals("http", ignoreCase = true) &&
                            !isPrivateHost(parsed.host)
                        ) {
                            error = "HTTP is only allowed for private/local IPs. Use https:// for public servers."
                            return@Button
                        }
                    } catch (_: Exception) {
                        error = "Invalid URL"
                        return@Button
                    }

                    scope.launch {
                        isLoading = true
                        error = null
                        try {
                            val result = downloadCertificate(url, passphrase)
                            result.fold(
                                onSuccess = { pairingResult ->
                                    onPairingComplete(
                                        pairingResult.serverAddress,
                                        pairingResult.grpcPort,
                                        pairingResult.certPem,
                                        pairingResult.keyPem,
                                        pairingResult.caCertPem,
                                        pairingResult.deviceName
                                    )
                                },
                                onFailure = { e ->
                                    error = e.message ?: "Failed to download certificate"
                                }
                            )
                        } finally {
                            isLoading = false
                        }
                    }
                },
                enabled = !isLoading && passphrase.isNotBlank() &&
                        ((mode == PairingMode.QR_SCAN && scannedUrl != null) ||
                                (mode == PairingMode.MANUAL && manualUrl.isNotBlank())),
                modifier = Modifier.fillMaxWidth()
            ) {
                if (isLoading) {
                    CircularProgressIndicator(
                        modifier = Modifier.size(24.dp),
                        color = MaterialTheme.colorScheme.onPrimary
                    )
                } else {
                    Icon(Icons.Default.Link, contentDescription = null)
                    Spacer(modifier = Modifier.width(8.dp))
                    Text("Connect")
                }
            }
        }
    }
}

/**
 * Returns true if [host] is a private/loopback/Tailscale IP address.
 * These are safe for cleartext HTTP during the short-lived LAN pairing flow.
 */
private fun isPrivateHost(host: String): Boolean {
    // Resolve hostname to numeric form if needed
    val addr = try {
        java.net.InetAddress.getByName(host)
    } catch (_: Exception) {
        return false
    }
    if (addr.isLoopbackAddress || addr.isLinkLocalAddress || addr.isSiteLocalAddress) {
        return true
    }
    // isSiteLocalAddress covers 10.*, 172.16-31.*, 192.168.* — but check Tailscale CGNAT range
    // manually: 100.64.0.0 – 100.127.255.255 (RFC 6598 shared-address space)
    val bytes = addr.address
    if (bytes.size == 4) {
        val b0 = bytes[0].toInt() and 0xFF
        val b1 = bytes[1].toInt() and 0xFF
        if (b0 == 100 && b1 in 64..127) return true
    }
    return false
}

private data class PairingResult(
    val serverAddress: String,
    val grpcPort: Int,
    val certPem: String,
    val keyPem: String,
    val caCertPem: String?,
    val deviceName: String
)

/**
 * Download the certificate from the pairing URL using the PEM endpoint.
 */
private suspend fun downloadCertificate(
    url: String,
    passphrase: String
): Result<PairingResult> {
    return kotlinx.coroutines.withContext(kotlinx.coroutines.Dispatchers.IO) {
        try {
            val parsedUrl = java.net.URL(url)
            val serverAddress = parsedUrl.host

            // POST to /pair/:token/pem with passphrase in JSON body
            val pemUrl = java.net.URL("$url/pem")
            Log.d("PairingScreen", "Connecting to $pemUrl")

            val connection = pemUrl.openConnection() as java.net.HttpURLConnection
            connection.requestMethod = "POST"
            connection.connectTimeout = 10000
            connection.readTimeout = 10000
            connection.doInput = true
            connection.doOutput = true
            connection.setRequestProperty("Content-Type", "application/json")

            // Send passphrase as JSON body
            val jsonBody = org.json.JSONObject().apply {
                put("passphrase", passphrase)
            }.toString()
            connection.outputStream.use { os ->
                os.write(jsonBody.toByteArray())
            }

            Log.d("PairingScreen", "Sending POST request...")
            val responseCode = connection.responseCode
            Log.d("PairingScreen", "Response code: $responseCode")

            if (responseCode != 200) {
                val errorStream = connection.errorStream?.bufferedReader()?.readText()
                Log.e("PairingScreen", "Error response: $errorStream")
                val errorMsg = try {
                    org.json.JSONObject(errorStream ?: "").optString("error", "Unknown error")
                } catch (_: Exception) { errorStream ?: "Server returned $responseCode" }
                return@withContext Result.failure(Exception(errorMsg))
            }

            val responseBody = connection.inputStream.bufferedReader().readText()
            connection.disconnect()

            val json = org.json.JSONObject(responseBody)
            val certPem = json.getString("cert")
            val keyPem = json.getString("key")
            val caCertPem = json.optString("ca_cert", null)
            val deviceName = json.getString("device_name")

            Log.d("PairingScreen", "Received cert for device: $deviceName, ca_cert: ${caCertPem != null}")

            Result.success(PairingResult(
                serverAddress = serverAddress,
                grpcPort = 50051,
                certPem = certPem,
                keyPem = keyPem,
                caCertPem = caCertPem,
                deviceName = deviceName
            ))
        } catch (e: Exception) {
            Log.e("PairingScreen", "Failed to download certificate", e)
            Result.failure(Exception("${e.javaClass.simpleName}: ${e.message}"))
        }
    }
}

@Composable
private fun QrScannerView(
    modifier: Modifier = Modifier,
    onQrScanned: (String) -> Unit
) {
    val context = LocalContext.current
    val lifecycleOwner = LocalLifecycleOwner.current
    var hasScanned by remember { mutableStateOf(false) }

    AndroidView(
        modifier = modifier,
        factory = { ctx ->
            val previewView = PreviewView(ctx)
            val cameraProviderFuture = ProcessCameraProvider.getInstance(ctx)

            cameraProviderFuture.addListener({
                val cameraProvider = cameraProviderFuture.get()

                val preview = Preview.Builder().build().also {
                    it.setSurfaceProvider(previewView.surfaceProvider)
                }

                val barcodeScanner = BarcodeScanning.getClient()
                val analysisExecutor = Executors.newSingleThreadExecutor()

                val imageAnalysis = ImageAnalysis.Builder()
                    .setBackpressureStrategy(ImageAnalysis.STRATEGY_KEEP_ONLY_LATEST)
                    .build()
                    .also { analysis ->
                        analysis.setAnalyzer(analysisExecutor) { imageProxy ->
                            if (hasScanned) {
                                imageProxy.close()
                                return@setAnalyzer
                            }

                            @androidx.camera.core.ExperimentalGetImage
                            val mediaImage = imageProxy.image
                            if (mediaImage != null) {
                                val image = InputImage.fromMediaImage(
                                    mediaImage,
                                    imageProxy.imageInfo.rotationDegrees
                                )
                                barcodeScanner.process(image)
                                    .addOnSuccessListener { barcodes ->
                                        for (barcode in barcodes) {
                                            if (barcode.valueType == Barcode.TYPE_URL ||
                                                barcode.valueType == Barcode.TYPE_TEXT
                                            ) {
                                                barcode.rawValue?.let { url ->
                                                    if (url.contains("/pair/") && !hasScanned) {
                                                        hasScanned = true
                                                        onQrScanned(url)
                                                    }
                                                }
                                            }
                                        }
                                    }
                                    .addOnCompleteListener {
                                        imageProxy.close()
                                    }
                            } else {
                                imageProxy.close()
                            }
                        }
                    }

                try {
                    cameraProvider.unbindAll()
                    cameraProvider.bindToLifecycle(
                        lifecycleOwner,
                        CameraSelector.DEFAULT_BACK_CAMERA,
                        preview,
                        imageAnalysis
                    )
                } catch (e: Exception) {
                    Log.e("QrScanner", "Camera binding failed", e)
                }
            }, ContextCompat.getMainExecutor(ctx))

            previewView
        }
    )
}

private enum class PairingMode {
    QR_SCAN,
    MANUAL
}
