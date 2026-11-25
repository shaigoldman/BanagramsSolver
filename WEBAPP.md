# Banana Board

The React/TypeScript app runs the Haskell solver in the browser. The solver is compiled to WebAssembly and runs in a Web Worker, so the UI stays responsive and no backend is required. The dictionary and the compiled solver are served as static files. Nothing entered in the app is sent to a solver server.

The app first runs the original search with up to 20 candidate states and 20 rounds. For hands of 14 or more tiles on devices reporting at least two logical cores, it distributes that search's board expansions across two WebAssembly workers; it falls back to the sequential implementation if workers are unavailable. The 14-tile cutoff is provisional and based on a small local benchmark. If it fails and every tile can appear in a dictionary word, it tries a mixed-scoring search with up to 35 states and 20 rounds. Progress identifies the current search and the result counts rounds from both stages. A “couldn't find a board” message therefore does not prove no board exists. There is no input length limit or automatic time limit. The search runs in a worker that can be stopped by the user. Progress reports the actual search round and elapsed time; round lengths vary, so it is not an estimate of time remaining.

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
# Optional longer search comparisons:
node tests/wasm-search-research.mjs
node --no-warnings tests/wasm-threshold-benchmark.mjs
npm run build --prefix web -- --base=/BanagramsSolver/
```

Use a local HTTP server to serve `web/dist` for testing; WebAssembly and Web Workers cannot reliably be loaded from a `file://` URL. The production build assumes the site's `/BanagramsSolver/` path.
