import { WASI, File, OpenFile, ConsoleStdout, PreopenDirectory } from '@bjorn3/browser_wasi_shim';

type Init = {type:'init'; module:WebAssembly.Module; words:ArrayBuffer};
type Task = {type:'task'; id:number; args:string[]};
let module:WebAssembly.Module | null = null;
let words:ArrayBuffer | null = null;

self.onmessage = async (event:MessageEvent<Init | Task>) => {
 const data=event.data;
 if(data.type==='init'){module=data.module;words=data.words;self.postMessage({type:'ready'});return}
 try {
  if(!module || !words)throw Error('Worker not initialized');
  let output='',errors='';
  const wasi=new WASI(['solver',...data.args],[],[
   new OpenFile(new File([])),
   ConsoleStdout.lineBuffered((line:string)=>{output+=line}),
   ConsoleStdout.lineBuffered((line:string)=>{
    const progress=/^PROGRESS (initial|fallback) (\d+) (\d+)$/.exec(line);
    if(progress)self.postMessage({type:'progress',id:data.id,stage:progress[1],round:Number(progress[2]),total:Number(progress[3])});
    else errors+=line;
   }),
   new PreopenDirectory('.',new Map([['words.txt',new File(new Uint8Array(words))]])),
  ]);
  const instance=await WebAssembly.instantiate(module,{wasi_snapshot_preview1:wasi.wasiImport});
  const code=wasi.start(instance as WebAssembly.Instance & {exports:{memory:WebAssembly.Memory;_start:()=>unknown}});
  if(code!==0)throw Error(errors || `Solver exited with code ${code}`);
  const result=JSON.parse(output);
  if(result.status==='error')throw Error(result.error);
  self.postMessage({type:'result',id:data.id,result});
 }catch(error){self.postMessage({type:'error',id:data.id,error:error instanceof Error?error.message:String(error)})}
};
