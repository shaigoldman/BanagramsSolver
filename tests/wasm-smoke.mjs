import { readFile } from 'node:fs/promises';
import { WASI, File, OpenFile, ConsoleStdout, PreopenDirectory } from '../web/node_modules/@bjorn3/browser_wasi_shim/dist/index.js';
const bytes=await readFile('web/public/solver.wasm');
const words=await readFile('words.txt');
const module=await WebAssembly.compile(bytes);
async function solve(letters) {
  let output='';
  const wasi=new WASI(['solver',letters],[],[new OpenFile(new File([])),ConsoleStdout.lineBuffered(s=>{output+=s}),ConsoleStdout.lineBuffered(s=>console.error(s)),new PreopenDirectory('.', new Map([['words.txt',new File(words)]]))]);
  const instance=await WebAssembly.instantiate(module, {wasi_snapshot_preview1:wasi.wasiImport});
  const exit=wasi.start(instance);
  if(exit!==0)throw Error(`WASM exit ${exit}`);
  return JSON.parse(output);
}
const solved=await solve('cat');
if(solved.status!=='solved'||solved.rows.join('').replaceAll(' ','').split('').sort().join('')!=='act')throw Error(JSON.stringify(solved));
const notFound=await solve('zzzz');
if(notFound.status!=='not_found')throw Error(JSON.stringify(notFound));
console.log('WASM solver checks passed');
