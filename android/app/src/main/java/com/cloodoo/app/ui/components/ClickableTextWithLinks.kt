// ClickableTextWithLinks.kt
//
// SPDX-License-Identifier: MIT
//
// Copyright (C) 2026 Anthony Green <green@moxielogic.com>

package com.cloodoo.app.ui.components

import android.content.Intent
import android.net.Uri
import android.util.Patterns
import androidx.compose.foundation.text.ClickableText
import androidx.compose.material3.MaterialTheme
import androidx.compose.runtime.Composable
import androidx.compose.ui.graphics.Color
import androidx.compose.ui.platform.LocalContext
import androidx.compose.ui.text.SpanStyle
import androidx.compose.ui.text.TextStyle
import androidx.compose.ui.text.buildAnnotatedString
import androidx.compose.ui.text.style.TextDecoration
import androidx.compose.ui.text.withStyle

@Composable
fun ClickableTextWithLinks(
    text: String,
    style: TextStyle,
    color: Color
) {
    val context = LocalContext.current
    val urlPattern = Patterns.WEB_URL
    val matcher = urlPattern.matcher(text)

    val annotatedString = buildAnnotatedString {
        var lastEnd = 0
        while (matcher.find()) {
            append(text.substring(lastEnd, matcher.start()))
            val url = matcher.group()
            pushStringAnnotation(tag = "URL", annotation = url)
            withStyle(
                style = SpanStyle(
                    color = MaterialTheme.colorScheme.primary,
                    textDecoration = TextDecoration.Underline
                )
            ) {
                append(url)
            }
            pop()
            lastEnd = matcher.end()
        }
        append(text.substring(lastEnd))
    }

    @Suppress("DEPRECATION")
    ClickableText(
        text = annotatedString,
        style = style.copy(color = color),
        onClick = { offset ->
            annotatedString.getStringAnnotations(tag = "URL", start = offset, end = offset)
                .firstOrNull()?.let { annotation ->
                    try {
                        val intent = Intent(Intent.ACTION_VIEW, Uri.parse(annotation.item))
                        context.startActivity(intent)
                    } catch (_: Exception) { }
                }
        }
    )
}
