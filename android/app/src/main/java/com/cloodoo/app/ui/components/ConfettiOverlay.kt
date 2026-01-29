package com.cloodoo.app.ui.components

import androidx.compose.foundation.layout.fillMaxSize
import androidx.compose.runtime.Composable
import androidx.compose.runtime.LaunchedEffect
import androidx.compose.ui.Modifier
import androidx.compose.ui.graphics.Color
import nl.dionsegijn.konfetti.compose.KonfettiView
import nl.dionsegijn.konfetti.core.Party
import nl.dionsegijn.konfetti.core.Position
import nl.dionsegijn.konfetti.core.emitter.Emitter
import java.util.concurrent.TimeUnit

@Composable
fun ConfettiOverlay(show: Boolean, onFinished: () -> Unit) {
    if (show) {
        KonfettiView(
            modifier = Modifier.fillMaxSize(),
            parties = listOf(
                Party(
                    speed = 0f,
                    maxSpeed = 30f,
                    damping = 0.9f,
                    spread = 360,
                    colors = listOf(
                        Color(0xFFE91E63).hashCode(),
                        Color(0xFF9C27B0).hashCode(),
                        Color(0xFF2196F3).hashCode(),
                        Color(0xFF4CAF50).hashCode(),
                        Color(0xFFFFEB3B).hashCode(),
                        Color(0xFFFF9800).hashCode()
                    ),
                    position = Position.Relative(0.5, 0.3),
                    emitter = Emitter(duration = 100, TimeUnit.MILLISECONDS).max(50)
                )
            )
        )
        LaunchedEffect(Unit) {
            kotlinx.coroutines.delay(2000)
            onFinished()
        }
    }
}
