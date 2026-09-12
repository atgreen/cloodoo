// PairingProof.kt
//
// SPDX-License-Identifier: MIT
//
// Copyright (C) 2026 Anthony Green <green@moxielogic.com>

package com.cloodoo.app.ui.util

import java.security.MessageDigest

/**
 * Compute the pairing passphrase proof sent to the server instead of the
 * raw passphrase: lowercase hex SHA-256 of the UTF-8 string
 * "cloodoo-pair-v1:" + passphrase.
 *
 * Must match pairing-passphrase-proof in src/certs.lisp. The raw
 * passphrase is still used locally to derive the bundle decryption key.
 */
fun pairingPassphraseProof(passphrase: String): String {
    val digest = MessageDigest.getInstance("SHA-256")
    val hash = digest.digest("cloodoo-pair-v1:$passphrase".toByteArray(Charsets.UTF_8))
    return hash.joinToString("") { "%02x".format(it) }
}
