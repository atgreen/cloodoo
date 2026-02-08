// AttachmentRepository.kt
//
// SPDX-License-Identifier: MIT
//
// Copyright (C) 2026 Anthony Green <green@moxielogic.com>

package com.cloodoo.app.data.repository

import android.content.Context
import android.net.Uri
import com.cloodoo.app.data.local.AttachmentDao
import com.cloodoo.app.data.local.AttachmentEntity
import java.io.File
import java.io.FileOutputStream
import java.io.InputStream
import java.security.MessageDigest
import java.time.ZonedDateTime
import java.time.format.DateTimeFormatter

/**
 * Repository for managing local attachment storage.
 * Handles storing, retrieving, and hashing attachment files.
 */
class AttachmentRepository(
    private val context: Context,
    private val attachmentDao: AttachmentDao
) {
    private val attachmentsDir: File by lazy {
        File(context.filesDir, "attachments").also { it.mkdirs() }
    }

    /**
     * Store an attachment locally from a content URI.
     * Returns the AttachmentEntity with computed hash.
     * Streams data while computing hash to avoid loading entire file into memory.
     */
    suspend fun storeLocally(
        uri: Uri,
        filename: String,
        mimeType: String
    ): AttachmentEntity {
        val inputStream = context.contentResolver.openInputStream(uri)
            ?: throw IllegalArgumentException("Cannot open URI: $uri")

        // Use a temporary file while we compute the hash
        val tempFile = File.createTempFile("attachment_", ".tmp", attachmentsDir)
        val digest = MessageDigest.getInstance("SHA-256")
        var fileSize = 0L

        try {
            // Stream data while computing hash and writing to temp file
            inputStream.use { input ->
                FileOutputStream(tempFile).use { output ->
                    val buffer = ByteArray(8192)
                    var bytesRead: Int
                    while (input.read(buffer).also { bytesRead = it } != -1) {
                        digest.update(buffer, 0, bytesRead)
                        output.write(buffer, 0, bytesRead)
                        fileSize += bytesRead
                    }
                }
            }

            val hash = digest.digest().joinToString("") { "%02x".format(it) }

            // Check if already exists
            val existing = attachmentDao.getByHash(hash)
            if (existing != null) {
                tempFile.delete()
                return existing
            }

            // Rename temp file to hash-based name
            val localFile = File(attachmentsDir, hash)
            tempFile.renameTo(localFile)

            val entity = AttachmentEntity(
                hash = hash,
                filename = filename,
                mimeType = mimeType,
                size = fileSize,
                createdAt = nowIso(),
                localPath = localFile.absolutePath,
                syncStatus = "pending_upload"
            )

            attachmentDao.insert(entity)
            return entity
        } catch (e: Exception) {
            tempFile.delete()
            throw e
        }
    }

    /**
     * Get the local file path for an attachment by its hash.
     * Returns null if the attachment is not cached locally.
     */
    suspend fun getLocalPath(hash: String): String? {
        val entity = attachmentDao.getByHash(hash) ?: return null
        val localPath = entity.localPath ?: return null
        val file = File(localPath)
        return if (file.exists()) localPath else null
    }

    /**
     * Get attachment entity by hash.
     */
    suspend fun getByHash(hash: String): AttachmentEntity? {
        return attachmentDao.getByHash(hash)
    }

    /**
     * Register an attachment that's already stored in app storage.
     * Used when PhotoAttachmentPicker has already saved the file.
     */
    suspend fun registerExisting(
        hash: String,
        filename: String,
        mimeType: String,
        size: Long,
        localPath: String
    ): AttachmentEntity {
        // Check if already registered
        val existing = attachmentDao.getByHash(hash)
        if (existing != null) {
            return existing
        }

        val entity = AttachmentEntity(
            hash = hash,
            filename = filename,
            mimeType = mimeType,
            size = size,
            createdAt = nowIso(),
            localPath = localPath,
            syncStatus = "pending_upload"
        )

        attachmentDao.insert(entity)
        return entity
    }

    /**
     * Get all attachments pending upload.
     */
    suspend fun getPendingUploads(): List<AttachmentEntity> {
        return attachmentDao.getPendingUploads()
    }

    /**
     * Update the sync status of an attachment.
     */
    suspend fun updateSyncStatus(hash: String, status: String) {
        attachmentDao.updateSyncStatus(hash, status)
    }

    /**
     * Store attachment content received from server.
     * Streams data while verifying hash to avoid loading entire file into memory.
     */
    suspend fun storeFromServer(
        hash: String,
        filename: String,
        mimeType: String,
        size: Long,
        contentStream: InputStream
    ): AttachmentEntity {
        val tempFile = File.createTempFile("attachment_", ".tmp", attachmentsDir)
        val digest = MessageDigest.getInstance("SHA-256")

        try {
            // Stream data while computing hash
            contentStream.use { input ->
                FileOutputStream(tempFile).use { output ->
                    val buffer = ByteArray(8192)
                    var bytesRead: Int
                    while (input.read(buffer).also { bytesRead = it } != -1) {
                        digest.update(buffer, 0, bytesRead)
                        output.write(buffer, 0, bytesRead)
                    }
                }
            }

            val computedHash = digest.digest().joinToString("") { "%02x".format(it) }
            if (computedHash != hash) {
                tempFile.delete()
                throw IllegalArgumentException("Hash mismatch: expected $hash, got $computedHash")
            }

            // Rename temp file to hash-based name
            val localFile = File(attachmentsDir, hash)
            tempFile.renameTo(localFile)

            val entity = AttachmentEntity(
                hash = hash,
                filename = filename,
                mimeType = mimeType,
                size = size,
                createdAt = nowIso(),
                localPath = localFile.absolutePath,
                syncStatus = "cached"
            )

            attachmentDao.insert(entity)
            return entity
        } catch (e: Exception) {
            tempFile.delete()
            throw e
        }
    }

    /**
     * Open an InputStream to read attachment content from local storage.
     * Caller is responsible for closing the stream.
     */
    suspend fun openContentStream(hash: String): InputStream? {
        val localPath = getLocalPath(hash) ?: return null
        val file = File(localPath)
        return if (file.exists()) file.inputStream() else null
    }

    /**
     * Compute SHA256 hash of content.
     */
    private fun computeHash(content: ByteArray): String {
        val digest = MessageDigest.getInstance("SHA-256")
        val hashBytes = digest.digest(content)
        return hashBytes.joinToString("") { "%02x".format(it) }
    }

    /**
     * Compute SHA256 hash from an input stream.
     */
    fun computeHash(inputStream: InputStream): String {
        val digest = MessageDigest.getInstance("SHA-256")
        val buffer = ByteArray(8192)
        var bytesRead: Int
        while (inputStream.read(buffer).also { bytesRead = it } != -1) {
            digest.update(buffer, 0, bytesRead)
        }
        val hashBytes = digest.digest()
        return hashBytes.joinToString("") { "%02x".format(it) }
    }

    private fun nowIso(): String {
        return ZonedDateTime.now().format(DateTimeFormatter.ISO_OFFSET_DATE_TIME)
    }
}
