# Banana Board for Android

This Android app opens the current hosted Banana Board in Android WebView. The solver still runs on the device via WebAssembly, including the browser-worker parallel search when supported. The app requires internet access to load the site; external links open in the system browser.

Install Android SDK platform 35 and build with JDK 17 and Gradle 8.10.2:

    cd android
    gradle assembleDebug lintDebug

The debug APK is under `app/build/outputs/apk/debug/`. GitHub Actions also uploads it as the `banana-board-android-debug` artifact. This is a debug-signed build for testing, not a signed Google Play release. A Play release needs a stable signing key, store assets and account setup.
