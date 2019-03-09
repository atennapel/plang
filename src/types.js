const { KFun, kType, pruneKind } = require('./kinds');

const TCon = (name, kind) => ({ tag: 'TCon', name, kind });
const TVar = (id, kind) => ({ tag: 'TVar', id, kind });
const TMeta = (id, kind) => ({ tag: 'TMeta', id, kind, type: null });
const TApp = (left, right) => ({ tag: 'TApp', left, right });

let _idTMeta = 0;
const resetTMeta = () => { _idTMeta = 0 };
const freshTMeta = kind => TMeta(_idTMeta++, kind);

const tFun = TCon('->', KFun(kType, KFun(kType, kType)));
const TFun = (left, right) => TApp(TApp(tFun, left), right);
const isTFun = t => t.tag === 'TApp' && t.left.tag === 'TApp' && t.left.left === tFun;

const showType = t => {
  if (t.tag === 'TCon') return t.name;
  if (t.tag === 'TVar') return `'${t.id}`;
  if (t.tag === 'TMeta') return `?${t.id}`;
  if (t.tag === 'TApp') return `(${showType(t.left)} ${showType(t.right)})`;
};

const flattenTApp = t => {
  let c = t;
  const r = [];
  while (c.tag === 'TApp') {
    r.push(c.right);
    c = c.left;
  }
  r.push(c);
  return r.reverse();
};

const flattenTFun = t => {
  let c = t;
  const r = [];
  while (isTFun(c)) {
    r.push(c.left.right);
    c = c.right;
  }
  return r;
};

const prettyType = t => {
  if (t.tag === 'TCon') return t.name;
  if (t.tag === 'TVar') return `'${t.id}`;
  if (t.tag === 'TMeta') return `?${t.id}`;
  if (isTFun(t))
    return flattenTFun(t)
      .map(x => isTFun(x) ? `(${prettyType(x)})` : prettyType(x))
      .join(' -> ');
  if (t.tag === 'TApp')
    return flattenTApp(t)
      .map(x => x.tag === 'TApp' ? `(${prettyType(x)})` : prettyType(x))
      .join(' ');
};

const pruneType = t => {
  if (t.tag === 'TCon') {
    t.kind = pruneKind(t.kind);
    return t;
  }
  if (t.tag === 'TVar') {
    t.kind = pruneKind(t.kind);
    return t;
  }
  if (t.tag === 'TMeta') {
    t.kind = pruneKind(t.kind);
    if (!t.type) return t;
    const ty = pruneType(t.type);
    t.type = ty;
    return ty;
  }
  if (t.tag === 'TApp') {
    const a = pruneType(t.left);
    const b = pruneType(t.right);
    return a !== t.left || b !== t.right ? TApp(a, b) : t;
  }
};

module.exports = {
  TCon,
  TVar,
  TMeta,
  TApp,

  resetTMeta,
  freshTMeta,

  tFun,
  TFun,
  isTFun,

  showType,

  flattenTApp,
  flattenTFun,
  prettyType,

  pruneType,
};
