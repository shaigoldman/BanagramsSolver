import { readFile } from 'node:fs/promises';
import { WASI, File, OpenFile, ConsoleStdout, PreopenDirectory } from '../web/node_modules/@bjorn3/browser_wasi_shim/dist/index.js';
const bytes=await readFile('web/public/solver.wasm');
const words=await readFile('words.txt');
const module=await WebAssembly.compile(bytes);
async function solve(letters, baseline = false, config = null) {
  let output=''; const progress=[];
  const args = config ? ['solver','--tuned',String(config.length),String(config.rounds),String(config.beam),JSON.stringify(config.weights),letters] : baseline ? ['solver','--baseline',letters] : ['solver',letters];
  const wasi=new WASI(args,[],[new OpenFile(new File([])),ConsoleStdout.lineBuffered(s=>{output+=s}),ConsoleStdout.lineBuffered(s=>{if(s.startsWith('PROGRESS '))progress.push(s);else console.error(s)}),new PreopenDirectory('.', new Map([['words.txt',new File(words)]]))]);
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


const baseWeights=[6833,13508,12218,10747,2298,12772,12985,8906,8034,14847,14228,10975,12594,8251,7493,13071,14905,9013,8673,5944,12242,14022,12640,14850,13026,14926];
const wide={length:15000,rounds:20,beam:35,weights:baseWeights};
const bag=Object.entries({a:13,b:3,c:3,d:6,e:18,f:3,g:4,h:3,i:12,j:2,k:2,l:5,m:3,n:8,o:11,p:3,q:2,r:9,s:6,t:9,u:6,v:3,w:3,x:2,y:3,z:2}).flatMap(([c,n])=>Array(n).fill(c));
let seed=20261128;
function random(n){seed=(Math.imul(seed,1664525)+1013904223)>>>0;return Math.floor(seed/4294967296*n)}
function hand(n){const tiles=bag.slice();let s='';for(let i=0;i<n;i++)s+=tiles.splice(random(tiles.length),1)[0];return s}
const cases=[12,17,21].flatMap(n=>Array.from({length:5},()=>({kind:'random',letters:hand(n)})));
cases.push({kind:'diagnostic',letters:'dadadadadadadadad'},{kind:'diagnostic',letters:'obqsyerepvkfieujn'},{kind:'diagnostic',letters:'fdilsnookasqtesei'});
function boardStats(result){
 if(result.status!=='solved')return {};
 const rows=result.rows,width=Math.max(...rows.map(r=>r.length));
 const lines=[...rows,...Array.from({length:width},(_,x)=>rows.map(r=>r[x]??' ').join(''))];
 const lengths=lines.flatMap(line=>line.split(/\s+/).filter(w=>w.length>1).map(w=>w.length));
 return {longest:Math.max(...lengths),meanLength:lengths.reduce((a,b)=>a+b,0)/lengths.length};
}
async function run(letters,mode){
 const start=performance.now();
 const first=await solve(letters,true);let result=first.result,rounds=first.progress.length,usedFallback=false;
 if(mode==='fallback' && result.status==='not_found'){
  usedFallback=true;const next=await solve(letters,false,wide);result=next.result;rounds+=next.progress.length;
 }
 validate(letters,result);
 return {mode,status:result.status,seconds:(performance.now()-start)/1000,rounds,usedFallback,...boardStats(result)};
}
console.log('BENCH_CASES',JSON.stringify(cases));
const records=[];
for(let i=0;i<cases.length;i++){
 for(let rep=0;rep<2;rep++){
  const modes=(i+rep)%2?['fallback','baseline']:['baseline','fallback'];
  for(const mode of modes){
   const row={...cases[i],rep,...await run(cases[i].letters,mode)};
   records.push(row);console.log('FALLBACK_RESULT',JSON.stringify(row));
  }
 }
}
for(const kind of ['random','diagnostic']) for(const mode of ['baseline','fallback']){
 const rs=records.filter(r=>r.kind===kind&&r.mode===mode),ok=rs.filter(r=>r.status==='solved');
 console.log('FALLBACK_SUMMARY',JSON.stringify({kind,mode,runs:rs.length,solved:ok.length,meanSeconds:rs.reduce((s,r)=>s+r.seconds,0)/rs.length,meanLongest:ok.reduce((s,r)=>s+r.longest,0)/ok.length,meanLength:ok.reduce((s,r)=>s+r.meanLength,0)/ok.length}));
}
