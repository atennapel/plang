const {
  showType,
  pruneType,
  tRowEmpty,
  isTRowExtend,
  freshTMeta,
  makeTRowExtend,
} = require('./types');
const { kType, kRow } = require('./kinds');
const { inferKind } = require('./kindInference');
const { unifyKinds } = require('./kindUnification');

const err = msg => { throw new TypeError(msg) };

const occurs = (v, t) => {
  if (v === t) return err(`occurs check failed: ${showType(v)} in ${showType(t)}`);
  if (t.tag === 'TApp') {
    occurs(v, t.left);
    occurs(v, t.right);
  }
};

const bind = (v, t) => {
  occurs(v, t);
  v.type = t;
};

const rewriteRow = (l, r) => {
  if (r === tRowEmpty) return err(`cannot find label ${l}`);
  if (r.tag === 'TMeta') {
    const tv = freshTMeta(kType);
    const tr = freshTMeta(kRow);
    const row = makeTRowExtend(l, tv, tr);
    bind(r, row);
    return row;
  }
  if (!isTRowExtend(r)) return err(`invalid type in rewriteRow(${l}): ${showType(r)}`);
  if (r.left.left.label === l) return r; 
  const rest = rewriteRow(l, r.right);
  return makeTRowExtend(l, rest.left.right,
    makeTRowExtend(r.left.left.label, r.left.right, rest.right));
};

const unify = (a_, b_) => {
  const a = pruneType(a_);
  const b = pruneType(b_);
  if (a === b) return;
  // console.log(`${showType(a)} ~ ${showType(b)}`);
  unifyKinds(inferKind(a), inferKind(b));
  if (a.tag === 'TMeta') return bind(a, b);
  if (b.tag === 'TMeta') return bind(b, a);
  if (isTRowExtend(a) && isTRowExtend(b)) {
    const br = rewriteRow(a.left.left.label, b);
    unify(a.left.right, br.left.right);
    return unify(a.right, br.right);
  }
  if (a.tag === 'TApp' && b.tag === 'TApp') {
    unify(a.left, b.left);
    return unify(a.right, b.right);
  }
  if (a.tag === 'TRowExtend' && b.tag === 'TRowExtend' && a.label === b.label)
    return;
  return err(`failed to unify types: ${showType(a)} ~ ${showType(b)}`);
};

module.exports = {
  unify,
};
