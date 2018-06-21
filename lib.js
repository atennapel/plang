const unit = null;

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

const Z = 0;
const S = x => x + 1;

function _rec(fz, fs, n) {
  let v = fz;
  for(let m = 0; m < n; m++) {
    v = fs(v)(m);
  }
  return v;
}
const rec = fz => fs => n => _rec(fz, fs, n);

const iff = c => a => b => c? a: b;

const pair = a => b => ({_tag: 'pair', _fst:a, _snd:b});
const fst = p => p._fst;
const snd = p => p._snd;

const nil = [];
const cons = h => t => [h].concat(t);

function _fold(fnil, fcons, a) {
  let v = fnil;
  for(let i = a.length - 1; i >= 0; i--) {
    v = fcons(v)(a[i]);
  }
  return v;
}
const fold = fnil => fcons => l => _fold(fnil, fcons, l);

const inl = v => ({ _tag: 'inl', _val: v });
const inr = v => ({ _tag: 'inr', _val: v });
const match = fa => fb => x => x._tag === 'inl'? fa(x._val): fb(x._val);
