# Add project specific ProGuard rules here.
# You can control the set of applied configuration files using the
# proguardFiles setting in build.gradle.kts.

# Keep Ktor
-keep class io.ktor.** { *; }
-dontwarn io.ktor.**

# Keep kotlinx.serialization
-keepattributes *Annotation*, InnerClasses
-dontnote kotlinx.serialization.AnnotationsKt
-keepclassmembers class kotlinx.serialization.json.** {
    *** Companion;
}
-keepclasseswithmembers class kotlinx.serialization.json.** {
    kotlinx.serialization.KSerializer serializer(...);
}
-keep,includedescriptorclasses class com.cloodoo.app.**$$serializer { *; }
-keepclassmembers class com.cloodoo.app.** {
    *** Companion;
}
-keepclasseswithmembers class com.cloodoo.app.** {
    kotlinx.serialization.KSerializer serializer(...);
}
