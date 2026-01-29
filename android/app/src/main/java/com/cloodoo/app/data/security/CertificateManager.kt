package com.cloodoo.app.data.security

import android.content.Context
import android.util.Base64
import android.util.Log
import javax.net.ssl.SSLContext
import javax.net.ssl.SSLSocketFactory
import java.io.ByteArrayInputStream
import java.io.File
import java.security.KeyFactory
import java.security.KeyStore
import java.security.cert.CertificateFactory
import java.security.cert.X509Certificate
import java.security.spec.PKCS8EncodedKeySpec
import javax.net.ssl.KeyManagerFactory
import javax.net.ssl.TrustManagerFactory

/**
 * Manages client certificates for mTLS authentication with the sync server.
 *
 * Certificates are stored as PEM files in the app's private directory.
 */
class CertificateManager(private val context: Context) {

    companion object {
        private const val TAG = "CertificateManager"
        private const val CERT_FILE = "client_cert.pem"
        private const val KEY_FILE = "client_key.pem"
        private const val CA_CERT_FILE = "ca_cert.pem"
        private const val PREFS_NAME = "cert_prefs"
        private const val PREF_SERVER_ADDRESS = "server_address"
        private const val PREF_SERVER_PORT = "server_port"
        private const val PREF_DEVICE_NAME = "device_name"
    }

    private val certPath: File
        get() = File(context.filesDir, CERT_FILE)

    private val keyPath: File
        get() = File(context.filesDir, KEY_FILE)

    private val caCertPath: File
        get() = File(context.filesDir, CA_CERT_FILE)

    private val prefs by lazy {
        context.getSharedPreferences(PREFS_NAME, Context.MODE_PRIVATE)
    }

    /**
     * Check if a client certificate is installed.
     */
    fun hasCertificate(): Boolean {
        return certPath.exists() && keyPath.exists()
    }

    /**
     * Import PEM certificate and key from the pairing endpoint.
     *
     * @param certPem The certificate in PEM format
     * @param keyPem The private key in PEM format (PKCS#1 RSA)
     * @param deviceName The device name from the server
     * @param serverAddress The server address to connect to
     * @param serverPort The gRPC port (default 50051)
     * @return Result indicating success or failure
     */
    fun importPemCertificate(
        certPem: String,
        keyPem: String,
        caCertPem: String? = null,
        deviceName: String,
        serverAddress: String,
        serverPort: Int = 50051
    ): Result<String> {
        return try {
            // Validate the certificate by parsing it
            val cert = parseCertificatePem(certPem)
            Log.d(TAG, "Certificate subject: ${cert.subjectX500Principal}")

            // Validate the key by parsing it
            parsePkcs1PrivateKey(keyPem)

            // Save PEM files
            certPath.writeText(certPem)
            keyPath.writeText(keyPem)
            // Save CA cert if provided (required for TLS verification of server)
            if (caCertPem != null) {
                parseCertificatePem(caCertPem) // validate
                caCertPath.writeText(caCertPem)
                Log.d(TAG, "CA certificate saved")
            } else {
                Log.w(TAG, "No CA certificate provided - TLS connection will not be possible")
            }
            // Restrict key file permissions
            keyPath.setReadable(false, false)
            keyPath.setReadable(true, true)
            keyPath.setWritable(false, false)
            keyPath.setWritable(true, true)

            // Save server configuration
            prefs.edit()
                .putString(PREF_SERVER_ADDRESS, serverAddress)
                .putInt(PREF_SERVER_PORT, serverPort)
                .putString(PREF_DEVICE_NAME, deviceName)
                .apply()

            Log.i(TAG, "PEM certificate imported successfully for device: $deviceName")
            Result.success(deviceName)
        } catch (e: Exception) {
            Log.e(TAG, "Failed to import PEM certificate", e)
            Result.failure(e)
        }
    }

    /**
     * Remove the installed certificate and server configuration.
     */
    fun removeCertificate() {
        if (certPath.exists()) certPath.delete()
        if (keyPath.exists()) keyPath.delete()
        if (caCertPath.exists()) caCertPath.delete()
        prefs.edit()
            .remove(PREF_SERVER_ADDRESS)
            .remove(PREF_SERVER_PORT)
            .remove(PREF_DEVICE_NAME)
            .apply()
        Log.i(TAG, "Certificate removed")
    }

    /**
     * Get the configured server address, or null if not configured.
     */
    fun getServerAddress(): String? {
        return prefs.getString(PREF_SERVER_ADDRESS, null)
    }

    /**
     * Get the configured server port.
     */
    fun getServerPort(): Int {
        return prefs.getInt(PREF_SERVER_PORT, 50051)
    }

    /**
     * Update the server address and port.
     */
    fun updateServerConfig(address: String, port: Int) {
        prefs.edit()
            .putString(PREF_SERVER_ADDRESS, address)
            .putInt(PREF_SERVER_PORT, port)
            .apply()
    }

    /**
     * Get the device name from the certificate.
     */
    fun getDeviceName(): String? {
        return prefs.getString(PREF_DEVICE_NAME, null)
    }

    /**
     * Create an SSLSocketFactory configured for mTLS with TLS 1.3.
     *
     * @return SSLSocketFactory configured for mTLS, or null if no certificate installed
     */
    fun createSslSocketFactory(): SSLSocketFactory? {
        if (!hasCertificate()) {
            Log.w(TAG, "No certificate installed")
            return null
        }

        return try {
            val certPem = certPath.readText()
            val keyPem = keyPath.readText()

            val cert = parseCertificatePem(certPem)
            val privateKey = parsePkcs1PrivateKey(keyPem)

            // Create in-memory KeyStore with the cert and key
            val keyStore = KeyStore.getInstance("PKCS12")
            keyStore.load(null, CharArray(0))
            keyStore.setKeyEntry(
                "client",
                privateKey,
                CharArray(0),
                arrayOf(cert)
            )

            // Set up key manager
            val keyManagerFactory = KeyManagerFactory.getInstance(
                KeyManagerFactory.getDefaultAlgorithm()
            )
            keyManagerFactory.init(keyStore, CharArray(0))

            // Trust the CA that signed the server's certificate
            if (!caCertPath.exists()) {
                Log.e(TAG, "No CA certificate found - cannot establish TLS connection")
                return null
            }
            val caCert = parseCertificatePem(caCertPath.readText())
            val trustStore = KeyStore.getInstance("PKCS12")
            trustStore.load(null, CharArray(0))
            trustStore.setCertificateEntry("ca", caCert)

            val trustManagerFactory = TrustManagerFactory.getInstance(
                TrustManagerFactory.getDefaultAlgorithm()
            )
            trustManagerFactory.init(trustStore)

            // Create SSLContext with TLS 1.3 explicitly
            val sslContext = SSLContext.getInstance("TLSv1.3")
            sslContext.init(
                keyManagerFactory.keyManagers,
                trustManagerFactory.trustManagers,
                null
            )
            sslContext.socketFactory
        } catch (e: Exception) {
            Log.e(TAG, "Failed to create SSL socket factory", e)
            null
        }
    }

    /**
     * Parse an X.509 certificate from PEM text.
     */
    private fun parseCertificatePem(pem: String): X509Certificate {
        val cf = CertificateFactory.getInstance("X.509")
        return cf.generateCertificate(ByteArrayInputStream(pem.toByteArray())) as X509Certificate
    }

    /**
     * Parse a PKCS#1 RSA private key from PEM text.
     * Converts PKCS#1 (RSA PRIVATE KEY) to PKCS#8 format for Java's KeyFactory.
     */
    private fun parsePkcs1PrivateKey(pem: String): java.security.PrivateKey {
        val b64 = pem.lines()
            .filter { !it.startsWith("-----") }
            .joinToString("")
        val pkcs1Bytes = Base64.decode(b64, Base64.DEFAULT)

        // Wrap PKCS#1 in PKCS#8 envelope
        val pkcs8Bytes = wrapPkcs1InPkcs8(pkcs1Bytes)

        val keyFactory = KeyFactory.getInstance("RSA")
        return keyFactory.generatePrivate(PKCS8EncodedKeySpec(pkcs8Bytes))
    }

    /**
     * Wrap PKCS#1 RSA key bytes in a PKCS#8 envelope.
     * PKCS#8 PrivateKeyInfo ::= SEQUENCE {
     *   version INTEGER (0),
     *   privateKeyAlgorithm AlgorithmIdentifier (RSA),
     *   privateKey OCTET STRING (containing PKCS#1 bytes)
     * }
     */
    private fun wrapPkcs1InPkcs8(pkcs1Bytes: ByteArray): ByteArray {
        // AlgorithmIdentifier for RSA: SEQUENCE { OID 1.2.840.113549.1.1.1, NULL }
        val algorithmId = byteArrayOf(
            0x30, 0x0d,       // SEQUENCE, length 13
            0x06, 0x09,       // OID, length 9
            0x2a, 0x86.toByte(), 0x48, 0x86.toByte(), 0xf7.toByte(),
            0x0d, 0x01, 0x01, 0x01, // 1.2.840.113549.1.1.1
            0x05, 0x00        // NULL
        )

        // version INTEGER 0
        val version = byteArrayOf(0x02, 0x01, 0x00)

        // OCTET STRING wrapping the PKCS#1 key
        val octetStringHeader = encodeDerLength(0x04, pkcs1Bytes.size)

        // Total content = version + algorithmId + octetString(pkcs1)
        val contentLength = version.size + algorithmId.size + octetStringHeader.size + pkcs1Bytes.size

        // Outer SEQUENCE
        val sequenceHeader = encodeDerLength(0x30, contentLength)

        // Assemble
        val result = ByteArray(sequenceHeader.size + contentLength)
        var offset = 0
        System.arraycopy(sequenceHeader, 0, result, offset, sequenceHeader.size)
        offset += sequenceHeader.size
        System.arraycopy(version, 0, result, offset, version.size)
        offset += version.size
        System.arraycopy(algorithmId, 0, result, offset, algorithmId.size)
        offset += algorithmId.size
        System.arraycopy(octetStringHeader, 0, result, offset, octetStringHeader.size)
        offset += octetStringHeader.size
        System.arraycopy(pkcs1Bytes, 0, result, offset, pkcs1Bytes.size)

        return result
    }

    /**
     * Encode a DER tag + length prefix.
     */
    private fun encodeDerLength(tag: Int, length: Int): ByteArray {
        return when {
            length < 128 -> byteArrayOf(tag.toByte(), length.toByte())
            length < 256 -> byteArrayOf(tag.toByte(), 0x81.toByte(), length.toByte())
            length < 65536 -> byteArrayOf(
                tag.toByte(), 0x82.toByte(),
                (length shr 8).toByte(), (length and 0xff).toByte()
            )
            else -> byteArrayOf(
                tag.toByte(), 0x83.toByte(),
                (length shr 16).toByte(),
                ((length shr 8) and 0xff).toByte(),
                (length and 0xff).toByte()
            )
        }
    }
}
