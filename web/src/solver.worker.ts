import { WASI, File, OpenFile, ConsoleStdout, PreopenDirectory } from '@bjorn3/browser_wasi_shim';
self.onmessage = async (event: MessageEvent<string>) => {
  try {
    const base = new URL(import.meta.env.BASE_URL, self.location.origin);
    const [wasmResponse, wordsResponse] = await Promise.all([fetch(new URL('solver.wasm', base)), fetch(new URL('words.txt', base))]);
    if (!wasmResponse.ok || !wordsResponse.ok) throw new Error('Could not load the solver.');
    const [wasmBytes, words] = await Promise.all([wasmResponse.arrayBuffer(), wordsResponse.arrayBuffer()]);
    let output = ''; let errors = '';
    const wasi = new WASI(['solver', event.data.toLowerCase()], [], [
      new OpenFile(new File([])),
      ConsoleStdout.lineBuffered((line: string) => { output += line; }),
      ConsoleStdout.lineBuffered((line: string) => { errors += line; }),
      new PreopenDirectory('.', new Map([['words.txt', new File(new Uint8Array(words))]])),
    ]);
    const module = await WebAssembly.compile(wasmBytes);
    const instance = await WebAssembly.instantiate(module, {wasi_snapshot_preview1: wasi.wasiImport});
    const exitCode = wasi.start(instance as WebAssembly.Instance & { exports: { memory: WebAssembly.Memory; _start: () => unknown } });
    if (exitCode !== 0) throw new Error(errors || `Solver exited with code ${exitCode}.`);
    self.postMessage(JSON.parse(output));
  } catch (error) {
    self.postMessage({ status: 'error', error: error instanceof Error ? error.message : 'Could not run the solver.' });
  }
};
