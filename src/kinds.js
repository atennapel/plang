const KCon = name => ({ tag: 'KCon', name });
const KMeta = id => ({ tag: 'KMeta', id, kind: null });
const KFun = (left, right) => ({ tag: 'KFun', left, right });

let _idKMeta = 0;
const resetKMeta = () => { _idKMeta = 0 };
const freshKMeta = () => KMeta(_idKMeta++);

const kType = KCon('Type');

const showKind = k => {
  if (k.tag === 'KCon') return k.name;
  if (k.tag === 'KMeta') return `?${k.id}`;
  if (k.tag === 'KFun') return `(${showKind(k.left)} -> ${showKind(k.right)})`;
};

const pruneKind = k => {
  if (k.tag === 'KMeta') {
    if (!k.kind) return k;
    const ki = pruneKind(k.kind);
    k.kind = ki;
    return ki;
  }
  if (k.tag === 'KFun') {
    const a = pruneKind(k.left);
    const b = pruneKind(k.right);
    return a !== k.left || b !== k.right ? KFun(a, b) : k;
  }
  return k;
};

module.exports = {
  KCon,
  KMeta,
  KFun,

  resetKMeta,
  freshKMeta,

  kType,

  showKind,

  pruneKind,
};
