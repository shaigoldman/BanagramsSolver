import { WASI, File, OpenFile, ConsoleStdout, PreopenDirectory } from '@bjorn3/browser_wasi_shim';

type BoardState={wire:string;done:boolean;rows:string[]};
type Reply={states:BoardState[]};
type Result={status:'solved';rows:string[]}|{status:'not_found'};
type Progress={status:'progress';phase:'searching';stage:'initial'|'fallback';round:number;total:number};
let clients:Client[]=[];
let cancelled=false;
let nextId=0;

class Client {
 private worker:Worker;
 private pending=new Map<number,{resolve:(value:any)=>void;reject:(error:Error)=>void}>();
 readonly ready:Promise<void>;
 constructor(module:WebAssembly.Module,words:ArrayBuffer){
  this.worker=new Worker(new URL('./parallel.part.worker.ts',import.meta.url),{type:'module'});
  this.ready=new Promise((resolve,reject)=>{
   this.worker.onmessage=(event:MessageEvent<any>)=>{
    const msg=event.data;
    if(msg.type==='ready'){resolve();return}
    if(msg.type==='progress'){self.postMessage({status:'progress',phase:'searching',stage:msg.stage,round:msg.round,total:msg.total} satisfies Progress);return}
    const pending=this.pending.get(msg.id);
    if(!pending)return;
    this.pending.delete(msg.id);
    if(msg.type==='error')pending.reject(new Error(msg.error));else pending.resolve(msg.result);
   };
   const fail=()=>{const error=new Error('Parallel worker failed');reject(error);for(const p of this.pending.values())p.reject(error);this.pending.clear()};
   this.worker.onerror=fail;
   this.worker.onmessageerror=fail;
   this.worker.postMessage({type:'init',module,words});
  });
 }
 request<T>(args:string[]):Promise<T>{
  return new Promise((resolve,reject)=>{const id=++nextId;this.pending.set(id,{resolve,reject});this.worker.postMessage({type:'task',id,args})});
 }
 stop(){this.worker.terminate()}
}

async function sequential(module:WebAssembly.Module,words:ArrayBuffer,letters:string):Promise<Result>{
 let output='',errors='';
 const wasi=new WASI(['solver',letters],[],[new OpenFile(new File([])),
  ConsoleStdout.lineBuffered((line:string)=>{output+=line}),
  ConsoleStdout.lineBuffered((line:string)=>{
   const p=/^PROGRESS (initial|fallback) (\d+) (\d+)$/.exec(line);
   if(p)self.postMessage({status:'progress',phase:'searching',stage:p[1],round:Number(p[2]),total:Number(p[3])});
   else errors+=line;
  }),
  new PreopenDirectory('.',new Map([['words.txt',new File(new Uint8Array(words))]]))]);
 const instance=await WebAssembly.instantiate(module,{wasi_snapshot_preview1:wasi.wasiImport});
 const exit=wasi.start(instance as WebAssembly.Instance & {exports:{memory:WebAssembly.Memory;_start:()=>unknown}});
 if(exit!==0)throw new Error(errors || `Solver exited with code ${exit}`);
 return JSON.parse(output);
}

async function distributed(letters:string):Promise<Result>{
 let {states}=await clients[0].request<Reply>(['--parallel-init',letters]);
 for(let round=1;round<=20;round++){
  self.postMessage({status:'progress',phase:'searching',stage:'initial',round,total:20} satisfies Progress);
  if(!states.length)break;
  const solved=states.find(s=>s.done);
  if(solved)return {status:'solved',rows:solved.rows};
  if(round===20)break;
  const batches=clients.map((_,i)=>{
   const start=Math.floor(i*states.length/clients.length),end=Math.floor((i+1)*states.length/clients.length);
   return states.slice(start,end).map(s=>s.wire);
  });
  const parts=await Promise.all(clients.map((client,i)=>batches[i].length?client.request<Reply>(['--parallel-expand',JSON.stringify(batches[i])]):Promise.resolve({states:[]})));
  const merged=parts.flatMap(part=>part.states.map(s=>s.wire));
  ({states}=await clients[0].request<Reply>(['--parallel-rank','20',JSON.stringify(merged)]));
 }
 // The first search is exhausted; reuse the existing dictionary check and
 // broader search without running the first stage again.
 return clients[0].request<Result>(['--fallback-only',letters]);
}

self.onmessage=async (event:MessageEvent<string>)=>{
 if(event.data==='__cancel__'){cancelled=true;clients.forEach(c=>c.stop());clients=[];return}
 cancelled=false;
 try{
  self.postMessage({status:'progress',phase:'loading'});
  const base=new URL(import.meta.env.BASE_URL,self.location.origin);
  const [wasmResponse,wordsResponse]=await Promise.all([fetch(new URL('solver.wasm',base)),fetch(new URL('words.txt',base))]);
  if(!wasmResponse.ok||!wordsResponse.ok)throw Error('Could not load the solver.');
  const [wasmBytes,words]=await Promise.all([wasmResponse.arrayBuffer(),wordsResponse.arrayBuffer()]);
  const module=await WebAssembly.compile(wasmBytes);
  const letters=event.data.toLowerCase();
  let result:Result;
  const count=Math.min(2,navigator.hardwareConcurrency || 1);
  if(count<2 || typeof Worker==='undefined'){
   result=await sequential(module,words,letters);
  }else{
   try{
    clients=Array.from({length:count},()=>new Client(module,words));
    await Promise.all(clients.map(c=>c.ready));
    result=await distributed(letters);
   }catch(error){
    if(cancelled)return;
    clients.forEach(c=>c.stop());clients=[];
    // Browser worker limitations should not prevent solving.
    result=await sequential(module,words,letters);
   }
  }
  clients.forEach(c=>c.stop());clients=[];
  if(!cancelled)self.postMessage(result);
 }catch(error){
  if(!cancelled)self.postMessage({status:'error',error:error instanceof Error?error.message:'Could not run the solver.'});
 }finally{clients.forEach(c=>c.stop());clients=[]}
};
