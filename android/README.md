# Banana Board for Android

The Android app packages the website, dictionary and WebAssembly solver. The solver works without network access. Links to the paper and GitHub open in the browser and require network access. Bundled assets update when a new app version is installed.

## Build

Requires JDK 17, Android SDK platform 36, Gradle 8.11.1, Node 20 and the GHC WASM toolchain. From the repository root:

```sh
npm ci --prefix web
rm -f BananaSolver.cabal banana-web.cabal
wasm32-wasi-cabal update
wasm32-wasi-cabal build exe:banana-wasm
mkdir -p web/public android/app/src/main/assets/www
cp "$(wasm32-wasi-cabal list-bin exe:banana-wasm)" web/public/solver.wasm
cp words.txt web/public/words.txt
npm run build --prefix web -- --base=/assets/www/
cp -R web/dist/. android/app/src/main/assets/www/
cd android
gradle assembleDebug lintDebug bundleRelease
```

The first `rm` only removes generated Cabal package descriptions from the checkout; use a disposable checkout, as in CI. GitHub Actions produces a debug-signed APK and an **unsigned** release AAB. Never submit the debug APK to Play. Create and securely retain a private upload key, configure Play App Signing, and sign the AAB with the upload key before submission. No signing key is included or stored in this repository.

The Android CI also runs instrumented tests on an emulator with Wi-Fi and mobile data disabled. Individual real phones and Android System WebView versions still need testing before release.
