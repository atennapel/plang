const { KFun, freshKMeta, kType, pruneKind } = require('./kinds');
const { unifyKinds } = require('./kindUnification');

const inferKind = t => {
  if (t.tag === 'TCon') return t.kind;
  if (t.tag === 'TMeta') return t.kind;
  if (t.tag === 'TApp') {
    const ka = inferKind(t.left);
    const kb = inferKind(t.right);
    const kr = freshKMeta();
    unifyKinds(ka, KFun(kb, kr));
    return pruneKind(kr);
  }
};

const checkKindType = t => unifyKinds(inferKind(t), kType);

module.exports = {
  inferKind,
  checkKindType,
};
