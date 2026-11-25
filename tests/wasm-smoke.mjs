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
const standard={length:15000,rounds:20,beam:20,weights:baseWeights};
const rareWeights=baseWeights.map((v,i)=>v+([9,16,23,25].includes(i)?8000:0));
// Shared class weights: vowels are the zero anchor because adding the
// same constant to every letter is equivalent to changing length weight.
const alphabet='abcdefghijklmnopqrstuvwxyz';
const groupWeights=(common,other,rare)=>[...alphabet].map(c=>
  'aeiou'.includes(c)?0:'lnrst'.includes(c)?common:'jkqxz'.includes(c)?rare:other);
let seed=20261127;
function randomInt(n){seed=(Math.imul(seed,1664525)+1013904223)>>>0;return Math.floor(seed / 4294967296 * n)}
const classCandidates=Array.from({length:8},(_,i)=>({
  name:'class_'+(i+1),
  length:10000+randomInt(4)*5000,
  rounds:[15,20,30][randomInt(3)],
  beam:[15,20,35][randomInt(3)],
  weights:groupWeights(randomInt(4)*2000,4000+randomInt(5)*2000,8000+randomInt(7)*2000)
}));
const configs=[{name:'mixed',...standard},...classCandidates];
console.log('TUNING_CONFIGS',JSON.stringify(configs));
const defaultCheck=(await solve('stonecats')).result;
const tunedCheck=(await solve('stonecats',false,standard)).result;
if(JSON.stringify(defaultCheck)!==JSON.stringify(tunedCheck)) throw Error('Default tuned configuration differs from mixed solver');
const train=['tsadmnimeroiyexrq','oohudieorjwojarma','ftogeiotrssdswaua','whxzdrluetrnrekmi','foiueioyvplasesos','yslearavneesidela','batazbejgvhwecusk','nkbaowxnaysolcidq','teendtugaamxateyy','rditgtmyizeptlgxo'];
const heldout=['ajnxeelqikdetahau','eabsvyltawiufpier','uetilwekdndtfdcaa','milnsiiadnetwvtfr','ijenaldgbzikrmbtl','keqqmcxtareenoest','sywsnpeaugvliiutd','devctelleiosxznnz','vsaguweiieaeeedyc','ttenloeaebasarlrs'];
function boardStats(result) {
 if(result.status!=='solved')return null;
 const rows=result.rows,width=Math.max(...rows.map(r=>r.length));
 const lines=[...rows,...Array.from({length:width},(_,x)=>rows.map(r=>r[x]??' ').join(''))];
 const lengths=lines.flatMap(line=>line.split(/\s+/).filter(w=>w.length>1).map(w=>w.length));
 return {longest:Math.max(...lengths),meanLength:lengths.reduce((a,b)=>a+b,0)/lengths.length};
}
const records=[];
async function run(split,name,hand,config,baseline=false) {
 const t=performance.now(), {result,progress}=await solve(hand,baseline,config);
 const seconds=(performance.now()-t)/1000; validate(hand,result);
 const row={split,name,hand,status:result.status,rounds:progress.length,seconds,...(boardStats(result)??{})};
 records.push(row);console.log('TUNING',JSON.stringify(row));
}
for(let i=0;i<train.length;i++){
 // Rotate the order to spread timing drift across configurations.
 const candidates=[null,...configs];
 for(let j=0;j<candidates.length;j++){
  const c=candidates[(j+i)%candidates.length];
  await run('train',c?.name??'baseline',train[i],c,!c);
 }
}
function summary(name,split) {
 const rs=records.filter(r=>r.name===name&&r.split===split), success=rs.filter(r=>r.status==='solved');
 return {name,split,solved:success.length,total:rs.length,meanLongest:success.length?success.reduce((s,r)=>s+r.longest,0)/success.length:0,meanWordLength:success.length?success.reduce((s,r)=>s+r.meanLength,0)/success.length:0,meanSeconds:rs.reduce((s,r)=>s+r.seconds,0)/rs.length};
}
const trainResults=configs.map(c=>summary(c.name,'train'));
trainResults.sort((a,b)=>b.solved-a.solved || b.meanLongest-a.meanLongest || a.meanSeconds-b.meanSeconds);
const winner=configs.find(c=>c.name===trainResults[0].name);
console.log('TRAIN_SELECTION',JSON.stringify({rule:'highest solve count, then mean longest word, then mean time',summaries:trainResults,winner:winner.name}));
for(let i=0;i<heldout.length;i++){
 const cs=[null,configs[0],...(winner.name==='mixed'?[]:[winner])];
 for(let j=0;j<cs.length;j++){const c=cs[(j+i)%cs.length];await run('holdout',c?.name??'baseline',heldout[i],c,!c)}
}
console.log('HELDOUT_SUMMARY',JSON.stringify(['baseline','mixed',...(winner.name==='mixed'?[]:[winner.name])].map(n=>summary(n,'holdout'))));
for(const hand of ['dadadadadadadadad','obqsyerepvkfieujn']) await run('diagnostic',winner.name,hand,winner);
