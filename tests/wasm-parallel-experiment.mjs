import { readFile } from 'node:fs/promises';
import { Worker, isMainThread, parentPort, workerData } from 'node:worker_threads';
import { availableParallelism } from 'node:os';
import { performance } from 'node:perf_hooks';

const weights = [6833,13508,12218,10747,2298,12772,12985,8906,8034,14847,14228,10975,12594,8251,7493,13071,14905,9013,8673,5944,12242,14022,12640,14850,13026,14926];

async function runWasm(letters, mode) {
  const { WASI, File, OpenFile, ConsoleStdout, PreopenDirectory } = await import('../web/node_modules/@bjorn3/browser_wasi_shim/dist/index.js');
  const [bytes, words] = await Promise.all([readFile('web/public/solver.wasm'), readFile('words.txt')]);
  const args = mode === 'baseline' ? ['solver','--baseline',letters]
    : mode === 'mixed' ? ['solver','--tuned','15000','20','35',JSON.stringify(weights),letters]
    : ['solver',letters];
  let output='', errors='';
  const wasi = new WASI(args, [], [new OpenFile(new File([])),
    ConsoleStdout.lineBuffered(s => { output += s; }),
    ConsoleStdout.lineBuffered(s => { if (!s.startsWith('PROGRESS ')) errors += s; }),
    new PreopenDirectory('.',new Map([['words.txt',new File(words)]]))]);
  const module = await WebAssembly.compile(bytes);
  const instance = await WebAssembly.instantiate(module, {wasi_snapshot_preview1:wasi.wasiImport});
  const exit = wasi.start(instance);
  if (exit !== 0) throw Error('WASM exit '+exit+': '+errors);
  return JSON.parse(output);
}

if (!isMainThread) {
  runWasm(workerData.letters,workerData.mode)
    .then(result => parentPort.postMessage({result}))
    .catch(error => parentPort.postMessage({error:String(error)}));
} else {
  function spawn(letters,mode) {
    const worker = new Worker(new URL(import.meta.url), {workerData:{letters,mode}});
    const result = new Promise((resolve,reject) => {
      worker.once('message', message => message.error ? reject(Error(message.error)) : resolve(message.result));
      worker.once('error',reject);
    });
    return {worker,result};
  }
  async function parallel(letters) {
    const a=spawn(letters,'baseline'), b=spawn(letters,'mixed');
    b.result.catch(() => {});
    try {
      const first=await a.result;
      if(first.status==='solved') return first;
      if(first.status!=='not_found')throw Error('Unexpected baseline result: '+JSON.stringify(first));
      return await b.result;
    } finally { await Promise.all([a.worker.terminate(),b.worker.terminate()]); }
  }
  const dictionary=new Set((await readFile('words.txt','utf8')).split(/\r?\n/));
  function verify(letters,result) {
    if (result.status!=='solved') throw Error('Expected solution for '+letters+': '+JSON.stringify(result));
    const rows=result.rows;
    if(rows.join('').replaceAll(' ','').split('').sort().join('')!==[...letters].sort().join(''))throw Error('Wrong tiles');
    const width=Math.max(...rows.map(r=>r.length));
    const lines=[...rows,...Array.from({length:width},(_,x)=>rows.map(r=>r[x]??' ').join(''))];
    for(const line of lines)for(const word of line.trim().split(/\s+/))if(word.length>1&&!dictionary.has(word))throw Error('Invalid word '+word);
    const cells=new Set();rows.forEach((r,y)=>[...r].forEach((c,x)=>{if(c!==' ')cells.add(y+','+x)}));
    const pending=[cells.values().next().value],visited=new Set(pending);
    while(pending.length){const [y,x]=pending.pop().split(',').map(Number);for(const [dy,dx] of [[0,1],[0,-1],[1,0],[-1,0]]){const p=(y+dy)+','+(x+dx);if(cells.has(p)&&!visited.has(p)){visited.add(p);pending.push(p)}}}
    if(visited.size!==cells.size)throw Error('Disconnected board');
  }
  console.log('PARALLEL_WORKER_BENCHMARK CPUs',availableParallelism());
  console.log('PARALLEL_METHOD one measured run per strategy; separate WASM instances; baseline result takes priority');
  for (const [index,letters] of ['stonecats','obqsyerepvkfieujn','dadadadadadadadad'].entries()) {
    for(const mode of (index%2?['parallel','serial']:['serial','parallel'])) {
      const t=performance.now();
      const result=mode==='serial' ? await runWasm(letters,'serial') : await parallel(letters);
      const seconds=(performance.now()-t)/1000;
      verify(letters,result);
      console.log('PARALLEL_RESULT',JSON.stringify({letters,mode,seconds,rows:result.rows}));
    }
  }
}
