const { KFun, kType, pruneKind } = require('./kinds');

const TCon = (name, kind) => ({ tag: 'TCon', name, kind });
const TMeta = (id, kind) => ({ tag: 'TMeta', id, kind, type: null });
const TApp = (left, right) => ({ tag: 'TApp', left, right });

let _idTMeta = 0;
const resetTMeta = () => { _idTMeta = 0 };
const freshTMeta = kind => TMeta(_idTMeta++, kind);

const tFun = TCon('->', KFun(kType, KFun(kType, kType)));
const TFun = (left, right) => TApp(TApp(tFun, left), right);

const showType = t => {
  if (t.tag === 'TCon') return t.name;
  if (t.tag === 'TMeta') return `?${t.id}`;
  if (t.tag === 'TApp') return `(${showType(t.left)} ${showType(t.right)})`;
};

const pruneType = t => {
  if (t.tag === 'TCon') {
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
  TMeta,
  TApp,

  resetTMeta,
  freshTMeta,

  tFun,
  TFun,

  showType,

  pruneType,
};
