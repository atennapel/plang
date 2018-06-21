function show(x) {
  if(x === null) return `()`;
  if(x._adt) return x._args.length === 0? `${x._tag}`: `(${x._tag}${x._args.length > 0? ` ${x._args.map(show).join(' ')}`: ''})`;
  if(Array.isArray(x)) return `[${x.map(show).join(', ')}]`;
  if(typeof x === 'function') return `[Function]`;
  if(x._tag === 'inl') return `(Inl ${show(x._val)})`;
  if(x._tag === 'inr') return `(Inr ${show(x._val)})`;
  if(x._tag === 'pair') return `(${show(x._fst)}, ${show(x._snd)})`;
  return `${x}`;
}
