import { readFile } from 'node:fs/promises';
import { WASI, File, OpenFile, ConsoleStdout, PreopenDirectory } from '../web/node_modules/@bjorn3/browser_wasi_shim/dist/index.js';
const bytes=await readFile('web/public/solver.wasm');
const words=await readFile('words.txt');
const module=await WebAssembly.compile(bytes);
async function solve(letters, baseline = false, config = null, fallback = false) {
  let output=''; const progress=[];
  const args = fallback ? ['solver','--fallback',letters] : config ? ['solver','--tuned',String(config.length),String(config.rounds),String(config.beam),JSON.stringify(config.weights),letters] : baseline ? ['solver','--baseline',letters] : ['solver',letters];
  const wasi=new WASI(args,[],[new OpenFile(new File([])),ConsoleStdout.lineBuffered(s=>{output+=s}),ConsoleStdout.lineBuffered(s=>{if(s.startsWith('PROGRESS '))progress.push(s);else console.error(s)}),new PreopenDirectory('.', new Map([['words.txt',new File(words)]]))]);
  const instance=await WebAssembly.instantiate(module, {wasi_snapshot_preview1:wasi.wasiImport});
  const exit=wasi.start(instance);
  if(exit!==0)throw Error(`WASM exit ${exit}`);
  return { result: JSON.parse(output), progress };
}
const {result:solved,progress} = await solve('cat');
if (!progress.some(line=>/^PROGRESS initial 1 20$/.test(line))) throw Error('Missing search progress');
if(solved.status!=='solved'||solved.rows.join('').replaceAll(' ','').split('').sort().join('')!=='act')throw Error(JSON.stringify(solved));
const {result:multi}=await solve('stonecats');
if(multi.status!=='solved' || multi.rows.length < 2 || multi.rows.join('').replaceAll(' ','').split('').sort().join('') !== 'stonecats'.split('').sort().join('')) throw Error(JSON.stringify(multi));
const {result:notFound}=await solve('z'.repeat(30));
if(notFound.status!=='not_found')throw Error(JSON.stringify(notFound));
console.log('WASM solver checks passed');

const dictionary = new Set(words.toString('utf8').split(/\r?\n/));
function validate(letters, result) {
  if (result.status !== 'solved') return;
  const rows = result.rows;
  const tiles = rows.join('').replaceAll(' ', '');
  if (tiles.split('').sort().join('') !== letters.split('').sort().join('')) throw Error('Wrong tiles: ' + letters);
  const width = Math.max(...rows.map(r => r.length));
  const lines = [...rows, ...Array.from({length: width}, (_, x) => rows.map(row => row[x] ?? ' ').join(''))];
  for (const line of lines) for (const word of line.trim().split(/\s+/)) {
    if (word.length > 1 && !dictionary.has(word)) throw Error('Invalid word: ' + word);
  }
  const points = new Set();
  rows.forEach((row,y) => [...row].forEach((c,x) => { if(c !== ' ') points.add(y+','+x); }));
  const todo = [points.values().next().value], visited = new Set(todo);
  while (todo.length) {
    const [y,x] = todo.pop().split(',').map(Number);
    for (const [dy,dx] of [[0,1],[0,-1],[1,0],[-1,0]]) {
      const k = (y+dy)+','+(x+dx);
      if(points.has(k) && !visited.has(k)){visited.add(k);todo.push(k)}
    }
  }
  if (visited.size !== points.size) throw Error('Disconnected board: ' + letters);
}




const fast = await solve('stonecats', true);
const normal = await solve('stonecats');
if (JSON.stringify(normal.result) !== JSON.stringify(fast.result)) throw Error('First search result changed');
if (normal.progress.some(line => /^PROGRESS fallback /.test(line))) throw Error('Fallback ran after a solution');
validate('stonecats', normal.result);

const hard = 'dadadadadadadadad';
const original = await solve(hard, true);
if (original.result.status !== 'not_found') throw Error('Expected baseline to miss D/A case');
const retried = await solve(hard);
if (retried.result.status !== 'solved') throw Error('Fallback failed to solve D/A case');
if (!retried.progress.some(line => /^PROGRESS initial /.test(line)) ||
    !retried.progress.some(line => /^PROGRESS fallback /.test(line))) throw Error('Missing stage progress');
validate(hard, retried.result);

const impossible = await solve('fdilsnookasqtesei');
if (impossible.result.status !== 'not_found') throw Error('Expected blocked Q hand to fail');
if (impossible.progress.some(line => /^PROGRESS fallback /.test(line))) throw Error('Unusable-letter check did not skip fallback');
console.log('Fallback regression checks passed');

// Measure independent WebAssembly instances in separate worker threads.
await import('./wasm-parallel-experiment.mjs');
