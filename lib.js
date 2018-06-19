const unit = null;
const impossible = () => { throw new Error('impossible') };

const Z = 0;
const S = x => x + 1;

function _rec(fz, fs, n) {
  let v = fz;
  for(let m = 0; m < n; m++) {
    v = fs(v)(m);
  }
  return v;
  // return n === 0? fz: fs(rec(fz, fs, n - 1))(n - 1);
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
  // return a.reduceRight((a, b) => fcons(a)(b), fnil);
}
const fold = fnil => fcons => l => _fold(fnil, fcons, l);

const inl = v => ({ _tag: 'inl', _val: v });
const inr = v => ({ _tag: 'inr', _val: v });
const match = fa => fb => x => x._tag === 'inl'? fa(x._val): fb(x._val);
