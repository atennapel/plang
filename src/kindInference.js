const { KFun, freshKMeta, kType, kRow, pruneKind } = require('./kinds');
const { showType } = require('./types');
const { unifyKinds } = require('./kindUnification');

const kRowExtend = KFun(kType, KFun(kRow, kRow));

const inferKind = t => {
  // console.log(`inferKind ${showType(t)}`);
  if (t.tag === 'TApp') {
    const ka = inferKind(t.left);
    const kb = inferKind(t.right);
    const kr = freshKMeta();
    unifyKinds(ka, KFun(kb, kr));
    return pruneKind(kr);
  }
  if (t.tag === 'TRowExtend') return kRowExtend;
  return t.kind;
};

const checkKindType = t => unifyKinds(inferKind(t), kType);

module.exports = {
  inferKind,
  checkKindType,
};
