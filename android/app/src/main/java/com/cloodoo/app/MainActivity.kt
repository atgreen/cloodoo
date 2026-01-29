// MainActivity.kt
//
// SPDX-License-Identifier: MIT
//
// Copyright (C) 2026 Anthony Green <green@moxielogic.com>

package com.cloodoo.app

import android.os.Bundle
import androidx.activity.ComponentActivity
import androidx.activity.compose.setContent
import androidx.activity.enableEdgeToEdge
import com.cloodoo.app.ui.navigation.CloodooApp
import com.cloodoo.app.ui.theme.CloodooTheme

class MainActivity : ComponentActivity() {
    override fun onCreate(savedInstanceState: Bundle?) {
        enableEdgeToEdge()
        super.onCreate(savedInstanceState)
        val openQuickAdd = intent?.getBooleanExtra("quick_add", false) == true
        setContent {
            CloodooTheme {
                CloodooApp(openQuickAdd = openQuickAdd)
            }
        }
    }
}
