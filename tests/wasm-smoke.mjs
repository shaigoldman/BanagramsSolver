import { readFile } from 'node:fs/promises';
import { WASI, File, OpenFile, ConsoleStdout, PreopenDirectory } from '../web/node_modules/@bjorn3/browser_wasi_shim/dist/index.js';
const bytes=await readFile('web/public/solver.wasm');
const words=await readFile('words.txt');
const module=await WebAssembly.compile(bytes);
async function solve(letters, baseline = false) {
  let output=''; const progress=[];
  const wasi=new WASI(baseline ? ['solver','--baseline',letters] : ['solver',letters],[],[new OpenFile(new File([])),ConsoleStdout.lineBuffered(s=>{output+=s}),ConsoleStdout.lineBuffered(s=>{if(s.startsWith('PROGRESS '))progress.push(s);else console.error(s)}),new PreopenDirectory('.', new Map([['words.txt',new File(words)]]))]);
  const instance=await WebAssembly.instantiate(module, {wasi_snapshot_preview1:wasi.wasiImport});
  const exit=wasi.start(instance);
  if(exit!==0)throw Error(`WASM exit ${exit}`);
  return { result: JSON.parse(output), progress };
}
const {result:solved,progress} = await solve('cat');
if (!progress.some(line=>/^PROGRESS 1 20$/.test(line))) throw Error('Missing search progress');
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
const hands = ['dadadadadadadadad','obqsyerepvkfieujn','ansziavrgtembnaal','fdilsnookasqtesei','rshanuceexrgnstla','afcneivolrrciiors','tbayefitlneariign','aaaatuyeamgloondt','pehwnrzqrtutnaeoa','briuspnhnantaeyej','tmdqubeantoodoogk'];
console.log('BENCHMARK mode,hand,status,rounds,seconds,rows');
for (let i=0; i<hands.length; i++) {
  for (const baseline of i%2 ? [false,true] : [true,false]) {
    const start = performance.now();
    const {result,progress} = await solve(hands[i],baseline);
    const seconds = (performance.now()-start)/1000;
    validate(hands[i],result);
    console.log('BENCHMARK',baseline?'baseline':'mixed',hands[i],result.status,progress.length,seconds.toFixed(3),JSON.stringify(result.rows ?? []));
  }
}
