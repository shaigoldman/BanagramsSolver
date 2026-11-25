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
const cfg = beam => ({length:15000,rounds:20,beam,weights:baseWeights});
const bag=Object.entries({a:13,b:3,c:3,d:6,e:18,f:3,g:4,h:3,i:12,j:2,k:2,l:5,m:3,n:8,o:11,p:3,q:2,r:9,s:6,t:9,u:6,v:3,w:3,x:2,y:3,z:2}).flatMap(([c,n])=>Array(n).fill(c));
let seed=20261129;
function random(n){seed=(Math.imul(seed,1664525)+1013904223)>>>0;return Math.floor(seed/4294967296*n)}
function hand(n){const tiles=bag.slice();let s='';for(let i=0;i<n;i++)s+=tiles.splice(random(tiles.length),1)[0];return s}
const cases=[12,17,21].flatMap(n=>Array.from({length:10},()=>({kind:'heldout',letters:hand(n)})));
cases.push({kind:'diagnostic',letters:'dadadadadadadadad'},{kind:'diagnostic',letters:'obqsyerepvkfieujn'},{kind:'diagnostic',letters:'fdilsnookasqtesei'});
const dictionaryWords=[...dictionary].filter(w=>/^[a-z]{2,}$/.test(w));
function impossibleLetters(hand){
 const counts=Array(26).fill(0),covered=new Set();
 for(const c of hand)counts[c.charCodeAt(0)-97]++;
 for(const w of dictionaryWords){
  const left=counts.slice();let valid=true;
  for(const c of w)if(--left[c.charCodeAt(0)-97]<0){valid=false;break}
  if(valid)for(const c of w)covered.add(c);
 }
 return [...new Set(hand)].filter(c=>!covered.has(c)).join('');
}
async function timed(letters,baseline,config){
 const t=performance.now();const {result,progress}=await solve(letters,baseline,config);
 const seconds=(performance.now()-t)/1000;validate(letters,result);
 if(!['not_found','solved'].includes(result.status))throw Error('Unexpected status '+JSON.stringify(result));
 return {status:result.status,seconds,rounds:progress.length,rows:result.rows??[]};
}
console.log('SEARCH_CASES',JSON.stringify(cases));
const records=[];
for(let i=0;i<cases.length;i++){
 const {kind,letters}=cases[i];
 const baseline=await timed(letters,true,null);
 let checkSeconds=0,missing='',retry25=null,retry35=null;
 if(baseline.status==='not_found'){
  const t=performance.now();missing=impossibleLetters(letters);checkSeconds=(performance.now()-t)/1000;
  if(!missing){
   // Rotate execution order to limit systematic timing bias.
   if(i%2){retry35=await timed(letters,false,cfg(35));retry25=await timed(letters,false,cfg(25))}
   else {retry25=await timed(letters,false,cfg(25));retry35=await timed(letters,false,cfg(35))}
  }
 }
 const row={kind,letters,baseline,missing,checkSeconds,retry25,retry35};
 records.push(row);console.log('SEARCH_RESULT',JSON.stringify(row));
}
for(const kind of ['heldout','diagnostic']){
 const rs=records.filter(x=>x.kind===kind);
 for(const mode of ['baseline','retry25','retry35']){
  const solved=rs.filter(r=>r.baseline.status==='solved'||(mode!=='baseline' && r[mode]?.status==='solved')).length;
  const seconds=rs.reduce((a,r)=>a+r.baseline.seconds+(mode==='baseline'?0:r.checkSeconds+(r[mode]?.seconds??0)),0);
  console.log('SEARCH_SUMMARY',JSON.stringify({kind,mode,hands:rs.length,solved,meanEstimatedSeconds:seconds/rs.length,impossible:rs.filter(r=>r.missing).length}));
 }
}
