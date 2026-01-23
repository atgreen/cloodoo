package com.cloodoo.app

import android.os.Bundle
import androidx.activity.ComponentActivity
import androidx.activity.compose.setContent
import androidx.compose.foundation.layout.*
import androidx.compose.material3.*
import androidx.compose.runtime.*
import androidx.compose.ui.Alignment
import androidx.compose.ui.Modifier
import androidx.compose.ui.platform.LocalContext
import androidx.compose.ui.text.input.PasswordVisualTransformation
import androidx.compose.ui.unit.dp
import com.cloodoo.app.data.security.CertificateManager
import com.cloodoo.app.ui.screens.PairingScreen
import com.cloodoo.app.ui.screens.TodoListScreen
import com.cloodoo.app.ui.theme.CloodooTheme

class MainActivity : ComponentActivity() {
    override fun onCreate(savedInstanceState: Bundle?) {
        super.onCreate(savedInstanceState)
        setContent {
            CloodooTheme {
                Surface(
                    modifier = Modifier.fillMaxSize(),
                    color = MaterialTheme.colorScheme.background
                ) {
                    MainContent()
                }
            }
        }
    }
}

@Composable
private fun MainContent() {
    val context = LocalContext.current
    val certificateManager = remember { CertificateManager(context) }

    var isPaired by remember { mutableStateOf(certificateManager.hasCertificate()) }
    // Try to load saved passphrase first (for app restarts)
    var passphrase by remember { mutableStateOf(certificateManager.getSavedPassphrase()) }
    var needsPassphrase by remember { mutableStateOf(isPaired && passphrase == null) }

    if (!isPaired) {
        PairingScreen(
            onPairingComplete = { serverAddress, port, pass, p12Data ->
                val result = certificateManager.importCertificate(
                    p12Data = p12Data,
                    passphrase = pass,
                    serverAddress = serverAddress,
                    serverPort = port
                )
                if (result.isSuccess) {
                    passphrase = pass
                    isPaired = true
                    needsPassphrase = false
                }
            },
            onCancel = {
                // User can cancel but will see pairing screen again
            }
        )
    } else if (needsPassphrase) {
        // Certificate exists but passphrase not saved (legacy install)
        PassphraseEntryScreen(
            certificateManager = certificateManager,
            onPassphraseEntered = { pass ->
                passphrase = pass
                needsPassphrase = false
            },
            onUnpair = {
                certificateManager.removeCertificate()
                isPaired = false
                needsPassphrase = false
            }
        )
    } else {
        TodoListScreen(
            certificateManager = certificateManager,
            passphrase = passphrase
        )
    }
}

@OptIn(ExperimentalMaterial3Api::class)
@Composable
private fun PassphraseEntryScreen(
    certificateManager: CertificateManager,
    onPassphraseEntered: (String) -> Unit,
    onUnpair: () -> Unit
) {
    var passphrase by remember { mutableStateOf("") }
    var error by remember { mutableStateOf<String?>(null) }
    var isVerifying by remember { mutableStateOf(false) }

    Scaffold(
        topBar = {
            TopAppBar(
                title = { Text("Enter Passphrase") }
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
            Text(
                "Certificate Found",
                style = MaterialTheme.typography.headlineSmall
            )
            Spacer(modifier = Modifier.height(8.dp))
            Text(
                "Please enter your passphrase to unlock the certificate",
                style = MaterialTheme.typography.bodyMedium,
                color = MaterialTheme.colorScheme.onSurfaceVariant
            )

            Spacer(modifier = Modifier.height(24.dp))

            OutlinedTextField(
                value = passphrase,
                onValueChange = {
                    passphrase = it
                    error = null
                },
                label = { Text("Passphrase") },
                placeholder = { Text("tiger-blue-forest") },
                modifier = Modifier.fillMaxWidth(),
                singleLine = true,
                visualTransformation = PasswordVisualTransformation(),
                isError = error != null
            )

            error?.let {
                Spacer(modifier = Modifier.height(8.dp))
                Text(
                    it,
                    color = MaterialTheme.colorScheme.error,
                    style = MaterialTheme.typography.bodySmall
                )
            }

            Spacer(modifier = Modifier.height(24.dp))

            Button(
                onClick = {
                    isVerifying = true
                    // Verify passphrase by trying to create credentials
                    val creds = certificateManager.createChannelCredentials(passphrase)
                    if (creds != null) {
                        // Save passphrase for future use
                        certificateManager.savePassphrase(passphrase)
                        onPassphraseEntered(passphrase)
                    } else {
                        error = "Invalid passphrase"
                    }
                    isVerifying = false
                },
                enabled = passphrase.isNotBlank() && !isVerifying,
                modifier = Modifier.fillMaxWidth()
            ) {
                if (isVerifying) {
                    CircularProgressIndicator(
                        modifier = Modifier.size(24.dp),
                        color = MaterialTheme.colorScheme.onPrimary
                    )
                } else {
                    Text("Unlock")
                }
            }

            Spacer(modifier = Modifier.height(16.dp))

            TextButton(onClick = onUnpair) {
                Text("Unpair and start fresh")
            }
        }
    }
}
