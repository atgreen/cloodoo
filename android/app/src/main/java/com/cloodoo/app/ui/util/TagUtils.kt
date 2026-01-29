package com.cloodoo.app.ui.util

import com.google.gson.Gson
import com.google.gson.reflect.TypeToken

fun parseTags(tagsStr: String?): List<String> {
    if (tagsStr.isNullOrBlank() || tagsStr == "null") return emptyList()
    return try {
        if (tagsStr.trimStart().startsWith("[")) {
            val type = object : TypeToken<List<String>>() {}.type
            Gson().fromJson<List<String>>(tagsStr, type) ?: emptyList()
        } else {
            tagsStr.split(",").map { it.trim() }.filter { it.isNotEmpty() }
        }
    } catch (e: Exception) {
        tagsStr.split(",").map { it.trim() }.filter { it.isNotEmpty() }
    }
}

fun tagsToStorageString(tags: List<String>): String {
    return tags.joinToString(",")
}
