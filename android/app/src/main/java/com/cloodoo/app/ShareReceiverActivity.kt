package com.cloodoo.app

import android.content.Intent
import android.os.Bundle
import android.widget.Toast
import androidx.activity.ComponentActivity
import androidx.lifecycle.lifecycleScope
import com.cloodoo.app.data.local.CloodooDatabase
import com.cloodoo.app.data.remote.SyncManager
import com.cloodoo.app.data.repository.TodoRepository
import com.cloodoo.app.data.security.CertificateManager
import kotlinx.coroutines.launch

class ShareReceiverActivity : ComponentActivity() {
    override fun onCreate(savedInstanceState: Bundle?) {
        super.onCreate(savedInstanceState)

        if (intent?.action == Intent.ACTION_SEND && intent.type == "text/plain") {
            val text = intent.getStringExtra(Intent.EXTRA_TEXT)
                ?: intent.getStringExtra(Intent.EXTRA_SUBJECT)

            if (!text.isNullOrBlank()) {
                val certificateManager = CertificateManager(this)
                if (!certificateManager.hasCertificate()) {
                    Toast.makeText(this, "Cloodoo not paired yet", Toast.LENGTH_SHORT).show()
                    finish()
                    return
                }

                val deviceId = certificateManager.getDeviceName() ?: "unknown"
                val database = CloodooDatabase.getDatabase(this)
                val repository = TodoRepository(database, deviceId)
                val syncManager = SyncManager(database, certificateManager, deviceId)

                lifecycleScope.launch {
                    val todo = repository.createTodo(title = text.trim())
                    syncManager.sendTodoUpsert(todo)
                    val display = if (text.length > 40) text.take(40) + "..." else text
                    Toast.makeText(
                        this@ShareReceiverActivity,
                        "Added: $display",
                        Toast.LENGTH_SHORT
                    ).show()
                    finish()
                }
            } else {
                Toast.makeText(this, "No text to share", Toast.LENGTH_SHORT).show()
                finish()
            }
        } else {
            finish()
        }
    }
}
