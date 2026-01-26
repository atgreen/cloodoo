package com.cloodoo.app

import android.os.Bundle
import androidx.activity.ComponentActivity
import androidx.activity.compose.setContent
import androidx.compose.foundation.layout.fillMaxSize
import androidx.compose.material3.MaterialTheme
import androidx.compose.material3.Surface
import androidx.compose.runtime.*
import androidx.compose.ui.Modifier
import androidx.compose.ui.platform.LocalContext
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

    if (!isPaired) {
        PairingScreen(
            onPairingComplete = { serverAddress, port, certPem, keyPem, caCertPem, deviceName ->
                val result = certificateManager.importPemCertificate(
                    certPem = certPem,
                    keyPem = keyPem,
                    caCertPem = caCertPem,
                    deviceName = deviceName,
                    serverAddress = serverAddress,
                    serverPort = port
                )
                if (result.isSuccess) {
                    isPaired = true
                }
            },
            onCancel = {
                // User can cancel but will see pairing screen again
            }
        )
    } else {
        TodoListScreen(
            certificateManager = certificateManager
        )
    }
}

