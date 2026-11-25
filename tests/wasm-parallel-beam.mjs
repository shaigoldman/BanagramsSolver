import { Worker, isMainThread, parentPort, workerData } from 'node:worker_threads';
import { readFile } from 'node:fs/promises';
import { performance } from 'node:perf_hooks';
import { availableParallelism } from 'node:os';
import { WASI, File, OpenFile, ConsoleStdout, PreopenDirectory } from '../web/node_modules/@bjorn3/browser_wasi_shim/dist/index.js';

const [bytes,words] = await Promise.all([readFile('web/public/solver.wasm'),readFile('words.txt')]);
const module = await WebAssembly.compile(bytes);
async function run(args) {
 let out='', err='';
 const wasi=new WASI(['solver',...args],[],[new OpenFile(new File([])),ConsoleStdout.lineBuffered(s=>{out+=s}),ConsoleStdout.lineBuffered(s=>{if(!s.startsWith('PROGRESS '))err+=s}),new PreopenDirectory('.',new Map([['words.txt',new File(words)]]))]);
 const instance=await WebAssembly.instantiate(module,{wasi_snapshot_preview1:wasi.wasiImport});
 const exit=wasi.start(instance);
 if(exit!==0)throw Error('WASM exit '+exit+': '+err);
 const result=JSON.parse(out);
 if(result.status==='error')throw Error(JSON.stringify(result));
 return result;
}
if(!isMainThread){
 parentPort.on('message',async ({id,args})=>{try{parentPort.postMessage({id,result:await run(args)})}catch(error){parentPort.postMessage({id,error:String(error)})}});
 parentPort.postMessage({ready:true});
}else{
 let requestId=0;
 function spawn() {
  const worker=new Worker(new URL(import.meta.url),{workerData:true});
  const pending=new Map();
  let readyResolve,readyReject;
  const ready=new Promise((resolve,reject)=>{readyResolve=resolve;readyReject=reject});
  worker.on('error',error=>{readyReject(error);for(const p of pending.values())p.reject(error);pending.clear()});
  worker.on('message',msg=>{if(msg.ready){readyResolve();return}const p=pending.get(msg.id);if(!p)return;pending.delete(msg.id);msg.error?p.reject(Error(msg.error)):p.resolve(msg.result)});
  return {worker,ready,request(args){return new Promise((resolve,reject)=>{const id=++requestId;pending.set(id,{resolve,reject});worker.postMessage({id,args})})}};
 }
 async function distributed(letters,count) {
  const workers=Array.from({length:count},spawn);
  try {
   await Promise.all(workers.map(w=>w.ready));
   let {states}=await workers[0].request(['--parallel-init',letters]);
   for(let round=1;round<=20;round++){
    if(!states.length)return {status:'not_found'};
    const found=states.find(s=>s.done);
    if(found)return {status:'solved',rows:found.rows};
    const batches=workers.map((w,i)=>{
      const start=Math.floor(i*states.length/workers.length),end=Math.floor((i+1)*states.length/workers.length);
      return states.slice(start,end).map(s=>s.wire);
    });
    const parts=await Promise.all(workers.map((w,i)=>batches[i].length?w.request(['--parallel-expand',JSON.stringify(batches[i])]):Promise.resolve({states:[]})));
    const merged=parts.flatMap(p=>p.states.map(s=>s.wire));
    ({states}=await workers[0].request(['--parallel-rank','20',JSON.stringify(merged)]));
   }
   return {status:'not_found'};
  }finally{await Promise.all(workers.map(w=>w.worker.terminate()))}
 }
 console.log('PARALLEL_BEAM_CPUS',availableParallelism());
 for(const letters of ['stonecats','cat','abcdefg','obqsyerepvkfieujn','dadadadadadadadad']){
  const t0=performance.now(), sequential=await run(['--baseline',letters]), t1=performance.now();
  const parallel=await distributed(letters,2), t2=performance.now();
  if(JSON.stringify(sequential)!==JSON.stringify(parallel))throw Error('Parallel result differs for '+letters+': '+JSON.stringify({sequential,parallel}));
  console.log('PARALLEL_BEAM_RESULT',JSON.stringify({letters,status:sequential.status,sequentialSeconds:(t1-t0)/1000,parallelSeconds:(t2-t1)/1000,rows:parallel.rows??[]}));
  if (sequential.status==='not_found') {
    const afterParallel=await run(['--fallback-only',letters]), whole=await run([letters]);
    if(JSON.stringify(afterParallel)!==JSON.stringify(whole))throw Error('Fallback-only differs for '+letters);
  }
 }
}
