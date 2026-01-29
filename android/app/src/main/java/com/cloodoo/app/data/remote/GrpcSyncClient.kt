// GrpcSyncClient.kt
//
// SPDX-License-Identifier: MIT
//
// Copyright (C) 2026 Anthony Green <green@moxielogic.com>

package com.cloodoo.app.data.remote

import android.util.Log
import com.cloodoo.app.data.security.CertificateManager
import com.cloodoo.app.proto.CloodooSync.*
import com.cloodoo.app.proto.TodoSyncGrpc
import io.grpc.ManagedChannel
import io.grpc.okhttp.OkHttpChannelBuilder
import io.grpc.stub.StreamObserver
import kotlinx.coroutines.channels.awaitClose
import kotlinx.coroutines.flow.Flow
import kotlinx.coroutines.flow.callbackFlow
import java.util.concurrent.TimeUnit

/**
 * gRPC client for bidirectional streaming sync with mTLS authentication.
 */
class GrpcSyncClient(
    private val certificateManager: CertificateManager
) {
    companion object {
        private const val TAG = "GrpcSyncClient"
    }

    private var managedChannel: ManagedChannel? = null
    private var requestObserver: StreamObserver<SyncMessage>? = null

    /**
     * Connect to the sync server and start bidirectional streaming.
     *
     * @param deviceId This device's unique ID
     * @param since Timestamp to sync from (ISO 8601), or null for full sync
     * @return Flow of incoming SyncMessages from the server
     */
    fun connect(deviceId: String, since: String?): Flow<SyncMessage> = callbackFlow {
        val serverAddress = certificateManager.getServerAddress()
            ?: throw IllegalStateException("Server address not configured")
        val serverPort = certificateManager.getServerPort()

        Log.i(TAG, "=== CONNECTING TO GRPC SERVER ===")
        Log.i(TAG, "Server address: $serverAddress")
        Log.i(TAG, "Server port: $serverPort")

        val sslSocketFactory = certificateManager.createSslSocketFactory()
            ?: throw IllegalStateException("Failed to create TLS credentials - certificate or CA may be missing")

        Log.i(TAG, "Using TLS 1.3 with mTLS client certificate")

        managedChannel = OkHttpChannelBuilder
            .forAddress(serverAddress, serverPort)
            .sslSocketFactory(sslSocketFactory)
            .tlsConnectionSpec(
                arrayOf("TLSv1.3"),
                arrayOf(
                    "TLS_AES_128_GCM_SHA256",
                    "TLS_AES_256_GCM_SHA384",
                    "TLS_CHACHA20_POLY1305_SHA256"
                )
            )
            // Accepted risk: hostname verification is disabled because the private CA
            // issues certificates with IP SANs and the server IP may change across
            // networks (e.g. Tailscale, LAN roaming). The CA-only trust anchor already
            // prevents MITM — only certificates signed by our private CA are accepted.
            .hostnameVerifier { _, _ -> true }
            .keepAliveTime(30, TimeUnit.SECONDS)
            .keepAliveTimeout(10, TimeUnit.SECONDS)
            .keepAliveWithoutCalls(true)
            .build()

        val stub = TodoSyncGrpc.newStub(managedChannel)

        // Create response observer
        val responseObserver = object : StreamObserver<SyncMessage> {
            override fun onNext(message: SyncMessage) {
                Log.d(TAG, "Received message: ${message.msgCase}")
                trySend(message)
            }

            override fun onError(t: Throwable) {
                Log.e(TAG, "=== STREAM ERROR ===")
                Log.e(TAG, "Error type: ${t.javaClass.name}")
                Log.e(TAG, "Error message: ${t.message}")
                Log.e(TAG, "Cause: ${t.cause?.message}")
                Log.e(TAG, "Stack trace:", t)
                close(t)
            }

            override fun onCompleted() {
                Log.d(TAG, "Stream completed")
                close()
            }
        }

        // Start bidirectional stream
        requestObserver = stub.syncStream(responseObserver)

        // Send init message with current time for clock skew check
        val clientTime = java.time.ZonedDateTime.now().format(
            java.time.format.DateTimeFormatter.ISO_OFFSET_DATE_TIME
        )
        val initMessage = SyncMessage.newBuilder()
            .setInit(
                SyncInit.newBuilder()
                    .setDeviceId(deviceId)
                    .setSince(since ?: "")
                    .setClientTime(clientTime)
                    .build()
            )
            .build()

        requestObserver?.onNext(initMessage)
        Log.d(TAG, "Sent init message with client_time=$clientTime")

        awaitClose {
            Log.d(TAG, "Flow closed, cleaning up")
            disconnect()
        }
    }

    /**
     * Send a todo change to the server.
     */
    fun sendChange(change: TodoChange) {
        val message = SyncMessage.newBuilder()
            .setChange(change)
            .build()

        requestObserver?.onNext(message)
        Log.d(TAG, "Sent change: ${change.changeCase}")
    }

    /**
     * Send an upsert for a todo.
     */
    fun sendUpsert(deviceId: String, todoData: TodoData) {
        val change = TodoChange.newBuilder()
            .setDeviceId(deviceId)
            .setTimestamp(java.time.ZonedDateTime.now().format(
                java.time.format.DateTimeFormatter.ISO_OFFSET_DATE_TIME
            ))
            .setUpsert(todoData)
            .build()

        sendChange(change)
    }

    /**
     * Send a delete request for a todo.
     */
    fun sendDelete(deviceId: String, todoId: String) {
        val change = TodoChange.newBuilder()
            .setDeviceId(deviceId)
            .setTimestamp(java.time.ZonedDateTime.now().format(
                java.time.format.DateTimeFormatter.ISO_OFFSET_DATE_TIME
            ))
            .setDeleteId(todoId)
            .build()

        sendChange(change)
    }

    /**
     * Disconnect from the server.
     */
    fun disconnect() {
        try {
            requestObserver?.onCompleted()
        } catch (e: Exception) {
            Log.w(TAG, "Error completing request stream", e)
        }
        requestObserver = null

        try {
            managedChannel?.shutdown()?.awaitTermination(5, TimeUnit.SECONDS)
        } catch (e: Exception) {
            Log.w(TAG, "Error shutting down channel", e)
        }
        managedChannel = null
    }

    /**
     * Check if connected.
     */
    fun isConnected(): Boolean {
        return managedChannel != null && !managedChannel!!.isShutdown
    }
}

/**
 * Extension to convert TodoData proto to a map for local storage.
 */
fun TodoData.toMap(): Map<String, Any?> = mapOf(
    "id" to id,
    "title" to title,
    "description" to description.ifEmpty { null },
    "priority" to priority,
    "status" to status,
    "scheduled_date" to scheduledDate.ifEmpty { null },
    "due_date" to dueDate.ifEmpty { null },
    "tags" to tagsList.joinToString(",").ifEmpty { null },
    "estimated_minutes" to if (estimatedMinutes > 0) estimatedMinutes else null,
    "url" to url.ifEmpty { null },
    "created_at" to createdAt,
    "completed_at" to completedAt.ifEmpty { null },
    "parent_id" to parentId.ifEmpty { null },
    "repeat_interval" to if (repeatInterval > 0) repeatInterval else null,
    "repeat_unit" to repeatUnit.ifEmpty { null }
)
