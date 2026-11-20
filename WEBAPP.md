# Banana Board

The React/TypeScript app runs the original Haskell solver in the browser. The solver is compiled to WebAssembly and runs in a Web Worker, so the UI stays responsive and no backend is required. The dictionary and the compiled solver are served as static files. Nothing entered in the app is sent to a solver server.

The original search considers at most 20 candidate states over at most 20 iterations. A “couldn't find a board” message therefore does not prove no board exists. The app stops a search after 20 seconds and accepts up to 25 English letters.

## Building

The GitHub Actions workflow `.github/workflows/wasm-check.yml` installs GHC's WebAssembly toolchain, compiles the solver, runs a smoke test through the WASI compatibility layer, builds a static bundle, and deploys it to GitHub Pages. The Pages build needs the `/BanagramsSolver/` base path.

To build locally, install the GHC WebAssembly toolchain from ghc-wasm-meta, Node.js, and npm. From the repository root:

```sh
# Isolate banana-wasm.cabal from the repository's other Cabal files in a temporary checkout.
wasm32-wasi-cabal update
wasm32-wasi-cabal build exe:banana-wasm
mkdir -p web/public
cp "$(wasm32-wasi-cabal list-bin exe:banana-wasm)" web/public/solver.wasm
cp words.txt web/public/words.txt
npm ci --prefix web
node tests/wasm-smoke.mjs
npm run build --prefix web -- --base=/BanagramsSolver/
```

Use a local HTTP server to serve `web/dist` for testing; WebAssembly and Web Workers cannot reliably be loaded from a `file://` URL. The production build assumes the site's `/BanagramsSolver/` path.
