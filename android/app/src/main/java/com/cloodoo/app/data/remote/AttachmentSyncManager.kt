// AttachmentSyncManager.kt
//
// SPDX-License-Identifier: MIT
//
// Copyright (C) 2026 Anthony Green <green@moxielogic.com>

package com.cloodoo.app.data.remote

import android.util.Log
import com.cloodoo.app.data.repository.AttachmentRepository
import com.cloodoo.app.data.security.CertificateManager
import com.cloodoo.app.proto.CloodooSync.*
import com.cloodoo.app.proto.AttachmentServiceGrpc
import com.google.protobuf.ByteString
import io.grpc.ManagedChannel
import io.grpc.okhttp.OkHttpChannelBuilder
import io.grpc.stub.StreamObserver
import kotlinx.coroutines.Dispatchers
import kotlinx.coroutines.suspendCancellableCoroutine
import kotlinx.coroutines.withContext
import java.io.ByteArrayOutputStream
import java.util.concurrent.TimeUnit
import kotlin.coroutines.resume
import kotlin.coroutines.resumeWithException

/**
 * Manages attachment upload/download via gRPC.
 * Attachments are uploaded after todo creation and downloaded on-demand.
 */
class AttachmentSyncManager(
    private val attachmentRepository: AttachmentRepository,
    private val certificateManager: CertificateManager
) {
    companion object {
        private const val TAG = "AttachmentSyncManager"
        private const val CHUNK_SIZE = 65536 // 64KB chunks
    }

    private var channel: ManagedChannel? = null
    private var stub: AttachmentServiceGrpc.AttachmentServiceStub? = null

    /**
     * Connect to the attachment service using the same credentials as the sync service.
     */
    fun connect() {
        disconnect()

        val serverAddress = certificateManager.getServerAddress()
            ?: throw IllegalStateException("Server address not configured")
        val serverPort = certificateManager.getServerPort()

        val sslSocketFactory = certificateManager.createSslSocketFactory()
            ?: throw IllegalStateException("Failed to create TLS credentials")

        channel = OkHttpChannelBuilder
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
            .hostnameVerifier { _, _ -> true }
            .keepAliveTime(30, TimeUnit.SECONDS)
            .keepAliveTimeout(10, TimeUnit.SECONDS)
            .build()

        stub = AttachmentServiceGrpc.newStub(channel)
        Log.d(TAG, "Connected to attachment service at $serverAddress:$serverPort")
    }

    /**
     * Disconnect from the attachment service.
     */
    fun disconnect() {
        channel?.shutdownNow()
        channel = null
        stub = null
    }

    /**
     * Upload all pending attachments to the server.
     */
    suspend fun uploadPendingAttachments() = withContext(Dispatchers.IO) {
        val pendingAttachments = attachmentRepository.getPendingUploads()
        Log.d(TAG, "Found ${pendingAttachments.size} pending attachments to upload")

        for (attachment in pendingAttachments) {
            try {
                uploadAttachment(
                    hash = attachment.hash,
                    filename = attachment.filename,
                    mimeType = attachment.mimeType,
                    size = attachment.size
                )
                attachmentRepository.updateSyncStatus(attachment.hash, "uploaded")
                Log.d(TAG, "Uploaded attachment: ${attachment.hash}")
            } catch (e: Exception) {
                Log.e(TAG, "Failed to upload attachment ${attachment.hash}", e)
            }
        }
    }

    /**
     * Upload a single attachment to the server.
     */
    private suspend fun uploadAttachment(
        hash: String,
        filename: String,
        mimeType: String,
        size: Long
    ) {
        val currentStub = stub ?: throw IllegalStateException("Not connected")

        val content = attachmentRepository.readContent(hash)
            ?: throw IllegalStateException("Attachment content not found")

        suspendCancellableCoroutine { continuation ->

        val responseObserver = object : StreamObserver<AttachmentUploadResponse> {
            override fun onNext(response: AttachmentUploadResponse) {
                if (response.error.isNotEmpty()) {
                    continuation.resumeWithException(RuntimeException(response.error))
                } else {
                    continuation.resume(response.hash)
                }
            }

            override fun onError(t: Throwable) {
                continuation.resumeWithException(t)
            }

            override fun onCompleted() {
                // Response already handled in onNext
            }
        }

        val requestObserver = currentStub.uploadAttachment(responseObserver)

            try {
                // Send metadata first
                val metadata = AttachmentMeta.newBuilder()
                    .setHash(hash)
                    .setFilename(filename)
                    .setMimeType(mimeType)
                    .setSize(size)
                    .build()

                val metadataRequest = AttachmentUploadRequest.newBuilder()
                    .setMetadata(metadata)
                    .build()
                requestObserver.onNext(metadataRequest)

                // Send content in chunks
                var offset = 0
                while (offset < content.size) {
                    val end = minOf(offset + CHUNK_SIZE, content.size)
                    val chunk = content.copyOfRange(offset, end)

                    val chunkRequest = AttachmentUploadRequest.newBuilder()
                        .setChunk(ByteString.copyFrom(chunk))
                        .build()
                    requestObserver.onNext(chunkRequest)

                    offset = end
                }

                requestObserver.onCompleted()
            } catch (e: Exception) {
                requestObserver.onError(e)
                throw e
            }
        }
    }

    /**
     * Fetch an attachment from the server on-demand.
     * Returns the local file path after caching.
     */
    suspend fun fetchAttachment(hash: String): String? = withContext(Dispatchers.IO) {
        // Check if already cached locally
        val localPath = attachmentRepository.getLocalPath(hash)
        if (localPath != null) {
            Log.d(TAG, "Attachment $hash already cached at $localPath")
            return@withContext localPath
        }

        if (stub == null) {
            Log.e(TAG, "Not connected to attachment service")
            return@withContext null
        }

        try {
            downloadAttachment(hash)
        } catch (e: Exception) {
            Log.e(TAG, "Failed to download attachment $hash", e)
            null
        }
    }

    /**
     * Download an attachment from the server.
     */
    private suspend fun downloadAttachment(hash: String): String? =
        suspendCancellableCoroutine { continuation ->
            val currentStub = stub ?: run {
                continuation.resumeWithException(IllegalStateException("Not connected"))
                return@suspendCancellableCoroutine
            }

            var metadata: AttachmentMeta? = null
            val contentBuffer = ByteArrayOutputStream()

            val request = AttachmentDownloadRequest.newBuilder()
                .setHash(hash)
                .build()

            currentStub.downloadAttachment(request, object : StreamObserver<AttachmentDownloadResponse> {
                override fun onNext(response: AttachmentDownloadResponse) {
                    if (response.error.isNotEmpty()) {
                        continuation.resumeWithException(RuntimeException(response.error))
                        return
                    }

                    when (response.responseCase) {
                        AttachmentDownloadResponse.ResponseCase.METADATA -> {
                            metadata = response.metadata
                            Log.d(TAG, "Received metadata for attachment $hash")
                        }
                        AttachmentDownloadResponse.ResponseCase.CHUNK -> {
                            contentBuffer.write(response.chunk.toByteArray())
                        }
                        else -> {}
                    }
                }

                override fun onError(t: Throwable) {
                    continuation.resumeWithException(t)
                }

                override fun onCompleted() {
                    val meta = metadata
                    if (meta == null) {
                        continuation.resumeWithException(RuntimeException("No metadata received"))
                        return
                    }

                    // Note: We use runBlocking here because we're inside a callback
                    // This is acceptable since we're already on IO dispatcher
                    kotlinx.coroutines.runBlocking {
                        try {
                            // Store locally
                            val content = contentBuffer.toByteArray()
                            attachmentRepository.storeFromServer(
                                hash = meta.hash,
                                filename = meta.filename,
                                mimeType = meta.mimeType,
                                size = meta.size,
                                content = content
                            )

                            val localPath = attachmentRepository.getLocalPath(hash)
                            Log.d(TAG, "Downloaded and cached attachment $hash at $localPath")
                            continuation.resume(localPath)
                        } catch (e: Exception) {
                            continuation.resumeWithException(e)
                        }
                    }
                }
            })
        }
}
