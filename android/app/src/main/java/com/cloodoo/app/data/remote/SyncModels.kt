package com.cloodoo.app.data.remote

import kotlinx.serialization.SerialName
import kotlinx.serialization.Serializable

/**
 * Response from GET /api/device
 */
@Serializable
data class DeviceResponse(
    @SerialName("device_id") val deviceId: String,
    @SerialName("server_time") val serverTime: String
)

/**
 * Response from GET /api/sync
 */
@Serializable
data class SyncGetResponse(
    @SerialName("device_id") val deviceId: String,
    @SerialName("server_time") val serverTime: String,
    val rows: List<SyncRow>
)

/**
 * Request body for POST /api/sync
 */
@Serializable
data class SyncPostRequest(
    @SerialName("device_id") val deviceId: String,
    val rows: List<SyncRow>
)

/**
 * Response from POST /api/sync
 */
@Serializable
data class SyncPostResponse(
    val accepted: Int,
    val rejected: Int,
    @SerialName("server_time") val serverTime: String
)

/**
 * A row in the sync payload - matches the desktop schema.
 */
@Serializable
data class SyncRow(
    @SerialName("row_id") val rowId: Long? = null,
    val id: String,
    val title: String,
    val description: String? = null,
    val priority: String = "medium",
    val status: String = "pending",
    @SerialName("scheduled_date") val scheduledDate: String? = null,
    @SerialName("due_date") val dueDate: String? = null,
    val tags: String? = null,
    @SerialName("estimated_minutes") val estimatedMinutes: Int? = null,
    @SerialName("location_info") val locationInfo: String? = null,
    val url: String? = null,
    @SerialName("parent_id") val parentId: String? = null,
    @SerialName("created_at") val createdAt: String,
    @SerialName("completed_at") val completedAt: String? = null,
    @SerialName("valid_from") val validFrom: String,
    @SerialName("valid_to") val validTo: String? = null,
    @SerialName("device_id") val deviceId: String
)
