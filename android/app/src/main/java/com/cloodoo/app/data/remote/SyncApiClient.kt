package com.cloodoo.app.data.remote

import io.ktor.client.*
import io.ktor.client.call.*
import io.ktor.client.engine.android.*
import io.ktor.client.plugins.contentnegotiation.*
import io.ktor.client.request.*
import io.ktor.http.*
import io.ktor.serialization.kotlinx.json.*
import kotlinx.serialization.json.Json

/**
 * HTTP client for communicating with the cloodoo server sync API.
 */
class SyncApiClient(
    private val baseUrl: String
) {
    private val client = HttpClient(Android) {
        install(ContentNegotiation) {
            json(Json {
                ignoreUnknownKeys = true
                isLenient = true
            })
        }
    }

    /**
     * Get the server's device ID and current time.
     */
    suspend fun getDevice(): Result<DeviceResponse> = runCatching {
        client.get("$baseUrl/api/device").body()
    }

    /**
     * Get all rows since a given timestamp.
     */
    suspend fun getSync(since: String? = null): Result<SyncGetResponse> = runCatching {
        client.get("$baseUrl/api/sync") {
            since?.let { parameter("since", it) }
        }.body()
    }

    /**
     * Send rows to the server for merging.
     */
    suspend fun postSync(request: SyncPostRequest): Result<SyncPostResponse> = runCatching {
        client.post("$baseUrl/api/sync") {
            contentType(ContentType.Application.Json)
            setBody(request)
        }.body()
    }

    fun close() {
        client.close()
    }
}
