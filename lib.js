function show(x) {
  if (typeof x === 'string') return JSON.stringify(x);
  if (typeof x === 'function') return '[Function]';
  if (typeof x._tag === 'string')
    return typeof x.val === 'undefined' ? x._tag :
      Array.isArray(x.val) ? `(${x._tag} ${x.val.map(show).join(' ')})` :
      `(${x._tag} ${show(x.val)})`;
  if (typeof x === 'object' && x._rec) {
    const r = [];
    for (let k in x) if (k[0] !== '_') r.push(`${k}: ${show(x[k])}`);
    return `{${r.join(', ')}}`;
  }
  return '' + x;
}

const end = () => { throw new Error('end') };

const fix = f => (x => f(y => x(x)(y)))(x => f(y => x(x)(y)));

const empty = { _rec: true };
const _select = l => r => r[l];
const _extend = l => v => r => {
  const n = {};
  for (let k in r) n[k] = r[k];
  n[l] = v;
  return n;
};
const _inject = l => val => ({ _tag: l, val });
const _case = l => fa => fb => x => x._tag === l ? fa(x.val) : fb(x);
