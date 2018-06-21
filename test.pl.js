;function show(x) {
  if(x === null) return `()`;
  if(x._adt) return x._args.length === 0? `${x._tag}`: `(${x._tag}${x._args.length > 0? ` ${x._args.map(show).join(' ')}`: ''})`;
  if(Array.isArray(x)) return `[${x.map(show).join(', ')}]`;
  if(typeof x === 'function') return `[Function]`;
  if(x._tag === 'inl') return `(Inl ${show(x._val)})`;
  if(x._tag === 'inr') return `(Inr ${show(x._val)})`;
  if(x._tag === 'pair') return `(${show(x._fst)}, ${show(x._snd)})`;
  return `${x}`;
};;const caseVoid = x=>{switch(x._tag){}throw new Error('case failed for Void')};;const Z = (({_adt:true,_tag:'Z',_args:[]}));const S = (x0=>({_adt:true,_tag:'S',_args:[x0]}));const caseNat = fZ=>fS=>x=>{switch(x._tag){case 'Z':return fZ;break;case 'S':return fS(x.args[0]);break;}throw new Error('case failed for Nat')};;const Nil = (({_adt:true,_tag:'Nil',_args:[]}));const Cons = (x0=>x1=>({_adt:true,_tag:'Cons',_args:[x0,x1]}));const caseList = fNil=>fCons=>x=>{switch(x._tag){case 'Nil':return fNil;break;case 'Cons':return fCons(x.args[0]),(x.args[1]);break;}throw new Error('case failed for List')};;const A = (x0=>({_adt:true,_tag:'A',_args:[x0]}));const caseA = fA=>x=>{switch(x._tag){case 'A':return fA(x.args[0]);break;}throw new Error('case failed for A')};;const main = A((x => Cons(x)(Nil)));console.log(show(main));