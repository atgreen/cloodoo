package com.cloodoo.app.ui.navigation

sealed class Screen(val route: String) {
    object Inbox : Screen("inbox")
    object Completed : Screen("completed")
    object Settings : Screen("settings")
    object AddTask : Screen("add_task")
    object Pairing : Screen("pairing")
    object VoiceAdd : Screen("voice_add")
    object OcrCapture : Screen("ocr_capture")
    object QuickAdd : Screen("quick_add")
}
