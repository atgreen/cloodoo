// PairingProofTest.kt
//
// SPDX-License-Identifier: MIT
//
// Copyright (C) 2026 Anthony Green <green@moxielogic.com>

package com.cloodoo.app.ui.util

import org.junit.Assert.assertEquals
import org.junit.Test

class PairingProofTest {

    @Test
    fun proofMatchesKnownVector() {
        // echo -n 'cloodoo-pair-v1:alpha-bravo' | sha256sum
        assertEquals(
            "37f2dba51c455f7fde480808b50a11d90588a2d561eca4ca52fabbd2075e389b",
            pairingPassphraseProof("alpha-bravo")
        )
    }

    @Test
    fun proofIsLowercaseHexOfSha256Length() {
        val proof = pairingPassphraseProof("anything")
        assertEquals(64, proof.length)
        assertEquals(proof.lowercase(), proof)
    }

    @Test
    fun proofOfEmptyPassphraseIsHashOfPrefixAlone() {
        // echo -n 'cloodoo-pair-v1:' | sha256sum
        assertEquals(
            "8f1eeb4a659da03594c6432c55a95c678dfbdb32a02ed126421fc7a4d014f004",
            pairingPassphraseProof("")
        )
    }
}
