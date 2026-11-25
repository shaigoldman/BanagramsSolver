import { WASI } from 'node:wasi';
import { readFileSync,openSync,closeSync,unlinkSync } from 'node:fs';
import { Worker, isMainThread, parentPort, threadId } from 'node:worker_threads';
import { performance } from 'node:perf_hooks';
import { tmpdir } from 'node:os';
const path=process.cwd(), module=await WebAssembly.compile(readFileSync(path+'/web/public/solver.wasm'));
let seq=0;
async function run(args){
 const tmp=tmpdir()+'/wasm-bench-'+process.pid+'-'+threadId+'-'+(++seq);const out=openSync(tmp,'w+'),err=openSync(tmp+'.err','w+');
 try{const wasi=new WASI({version:'preview1',args:['solver',...args],env:{},preopens:{'.':path},stdout:out,stderr:err,returnOnExit:true});const inst=await WebAssembly.instantiate(module,wasi.getImportObject());let code=wasi.start(inst);closeSync(out);closeSync(err);if(code!==0)throw Error('Exit '+code+': '+readFileSync(tmp+'.err','utf8'));const data=JSON.parse(readFileSync(tmp,'utf8'));if(data.status==='error')throw Error(JSON.stringify(data));return data}finally{try{closeSync(out)}catch{}try{closeSync(err)}catch{}try{unlinkSync(tmp)}catch{}try{unlinkSync(tmp+'.err')}catch{}}
}
if(!isMainThread){parentPort.on('message',async({id,args})=>{try{parentPort.postMessage({id,result:await run(args)})}catch(e){parentPort.postMessage({id,error:String(e)})}});parentPort.postMessage({ready:true})}
else{
 let id=0;
 function spawn(){const worker=new Worker(new URL(import.meta.url));let resolveReady,rejectReady;const ready=new Promise((resolve,reject)=>{resolveReady=resolve;rejectReady=reject}),pending=new Map();worker.on('error',e=>{rejectReady(e);for(const p of pending.values())p.reject(e)});worker.on('message',m=>{if(m.ready){resolveReady();return}let p=pending.get(m.id);if(!p)return;pending.delete(m.id);m.error?p.reject(Error(m.error)):p.resolve(m.result)});return{worker,ready,request(args){return new Promise((resolve,reject)=>{let n=++id;pending.set(n,{resolve,reject});worker.postMessage({id:n,args})})}}}
 async function parallel(letters){let workers=[spawn(),spawn()];try{await Promise.all(workers.map(w=>w.ready));let {states}=await workers[0].request(['--parallel-init',letters]);for(let round=1;round<=20;round++){if(!states.length)return {status:'not_found'};let done=states.find(s=>s.done);if(done)return{status:'solved',rows:done.rows};if(round===20)break;let parts=await Promise.all(workers.map((w,i)=>{let a=Math.floor(i*states.length/workers.length),b=Math.floor((i+1)*states.length/workers.length),v=states.slice(a,b).map(s=>s.wire);return v.length?w.request(['--parallel-expand',JSON.stringify(v)]):Promise.resolve({states:[]})}));let merged=parts.flatMap(p=>p.states.map(s=>s.wire));({states}=await workers[0].request(['--parallel-rank','20',JSON.stringify(merged)]))}return {status:'not_found'}}finally{await Promise.all(workers.map(w=>w.worker.terminate()))}}
 for(const letters of ['eboomdfnah', 'eiopkcvahr', 'nhgciodjnl', 'eclsxnelaoby', 'tyhrgueeidac', 'rtekitohcvlh', 'gbieoioaslaliu', 'sstuotrduznrrz', 'pnrasomuyundto', 'weohtohanneldgi', 'iubgncuthagiats', 'triuixqaizroxnz', 'uargnqodlfehwagm', 'aoreaupoulamwrtd', 'ieoapktueimelcmg', 'ikhitbfeeaasttfzdt', 'enajbllidnprwtoaea', 'iiarganiapaeweoyne']){let t=performance.now(),s=await run(['--baseline',letters]),m=performance.now(),p=await parallel(letters),e=performance.now();if(JSON.stringify(s)!==JSON.stringify(p))throw Error('Mismatch '+letters);console.log(JSON.stringify({letters,tiles:letters.length,status:s.status,sequential:+((m-t)/1000).toFixed(3),parallel:+((e-m)/1000).toFixed(3)}))}
}
