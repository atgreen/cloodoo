// Screen.kt
//
// SPDX-License-Identifier: MIT
//
// Copyright (C) 2026 Anthony Green <green@moxielogic.com>

package com.cloodoo.app.ui.navigation

sealed class Screen(val route: String) {
    object Inbox : Screen("inbox")
    object Completed : Screen("completed")
    object Settings : Screen("settings")
    object AddTask : Screen("add_task")
    object EditTask : Screen("edit_task/{todoId}") {
        fun createRoute(todoId: String) = "edit_task/$todoId"
    }
    object Pairing : Screen("pairing")
    object VoiceAdd : Screen("voice_add")
    object OcrCapture : Screen("ocr_capture")
    object QuickAdd : Screen("quick_add")
}
