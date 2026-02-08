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
import java.security.cert.X509Certificate
import java.util.concurrent.TimeUnit
import javax.net.ssl.HostnameVerifier
import javax.net.ssl.SSLSession
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
            .hostnameVerifier(createIpSanVerifier(serverAddress))
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
     * Streams from file in chunks to avoid loading entire file into memory.
     */
    private suspend fun uploadAttachment(
        hash: String,
        filename: String,
        mimeType: String,
        size: Long
    ) {
        val currentStub = stub ?: throw IllegalStateException("Not connected")

        val contentStream = attachmentRepository.openContentStream(hash)
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

                // Stream content in chunks from file
                contentStream.use { input ->
                    val buffer = ByteArray(CHUNK_SIZE)
                    var bytesRead: Int
                    while (input.read(buffer).also { bytesRead = it } != -1) {
                        val chunkRequest = AttachmentUploadRequest.newBuilder()
                            .setChunk(ByteString.copyFrom(buffer, 0, bytesRead))
                            .build()
                        requestObserver.onNext(chunkRequest)
                    }
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
     * Streams chunks directly to file to avoid loading entire file into memory.
     */
    private suspend fun downloadAttachment(hash: String): String? =
        suspendCancellableCoroutine { continuation ->
            val currentStub = stub ?: run {
                continuation.resumeWithException(IllegalStateException("Not connected"))
                return@suspendCancellableCoroutine
            }

            var metadata: AttachmentMeta? = null

            // Use PipedOutputStream/PipedInputStream for streaming from gRPC to repository
            val pipedOutputStream = java.io.PipedOutputStream()
            val pipedInputStream = java.io.PipedInputStream(pipedOutputStream)

            val request = AttachmentDownloadRequest.newBuilder()
                .setHash(hash)
                .build()

            currentStub.downloadAttachment(request, object : StreamObserver<AttachmentDownloadResponse> {
                override fun onNext(response: AttachmentDownloadResponse) {
                    if (response.error.isNotEmpty()) {
                        try {
                            pipedOutputStream.close()
                        } catch (_: Exception) {}
                        continuation.resumeWithException(RuntimeException(response.error))
                        return
                    }

                    try {
                        when (response.responseCase) {
                            AttachmentDownloadResponse.ResponseCase.METADATA -> {
                                metadata = response.metadata
                                Log.d(TAG, "Received metadata for attachment $hash")
                            }
                            AttachmentDownloadResponse.ResponseCase.CHUNK -> {
                                pipedOutputStream.write(response.chunk.toByteArray())
                            }
                            else -> {}
                        }
                    } catch (e: Exception) {
                        Log.e(TAG, "Error writing chunk", e)
                        continuation.resumeWithException(e)
                    }
                }

                override fun onError(t: Throwable) {
                    try {
                        pipedOutputStream.close()
                    } catch (_: Exception) {}
                    continuation.resumeWithException(t)
                }

                override fun onCompleted() {
                    val meta = metadata
                    if (meta == null) {
                        try {
                            pipedOutputStream.close()
                        } catch (_: Exception) {}
                        continuation.resumeWithException(RuntimeException("No metadata received"))
                        return
                    }

                    // Close the output stream to signal EOF to the input stream
                    try {
                        pipedOutputStream.close()
                    } catch (_: Exception) {}

                    // Note: We use runBlocking here because we're inside a callback
                    // This is acceptable since we're already on IO dispatcher
                    kotlinx.coroutines.runBlocking {
                        try {
                            // Store locally (streams from pipedInputStream)
                            attachmentRepository.storeFromServer(
                                hash = meta.hash,
                                filename = meta.filename,
                                mimeType = meta.mimeType,
                                size = meta.size,
                                contentStream = pipedInputStream
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

/**
 * Creates a hostname verifier that validates IP addresses in certificate SANs.
 * This is needed because the private CA issues certificates with IP SANs rather than DNS names,
 * and the server IP may change across networks (Tailscale, LAN roaming).
 *
 * @param expectedIp The IP address we expect to find in the certificate's SAN
 * @return A HostnameVerifier that validates the certificate contains the expected IP
 */
private fun createIpSanVerifier(expectedIp: String): HostnameVerifier {
    return HostnameVerifier { _, session ->
        try {
            val peerCertificates = session.peerCertificates
            if (peerCertificates.isEmpty()) {
                Log.e("AttachmentSyncManager", "No peer certificates in session")
                return@HostnameVerifier false
            }

            val cert = peerCertificates[0] as? X509Certificate
            if (cert == null) {
                Log.e("AttachmentSyncManager", "Peer certificate is not X509")
                return@HostnameVerifier false
            }

            // Get Subject Alternative Names (type 7 = IP address)
            val subjectAltNames = cert.subjectAlternativeNames
            if (subjectAltNames == null) {
                Log.e("AttachmentSyncManager", "Certificate has no Subject Alternative Names")
                return@HostnameVerifier false
            }

            // Look for IP address SANs (type 7) matching the expected IP
            for (san in subjectAltNames) {
                val type = san[0] as? Int
                val value = san[1] as? String

                if (type == 7 && value == expectedIp) {
                    Log.d("AttachmentSyncManager", "Certificate IP SAN verified: $expectedIp")
                    return@HostnameVerifier true
                }
            }

            Log.e("AttachmentSyncManager", "Certificate does not contain expected IP $expectedIp in SANs")
            return@HostnameVerifier false
        } catch (e: Exception) {
            Log.e("AttachmentSyncManager", "Error verifying hostname", e)
            return@HostnameVerifier false
        }
    }
}
