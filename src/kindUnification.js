const {
  showKind,
  pruneKind,
} = require('./kinds');

const err = msg => { throw new TypeError(msg) };

const occurs = (v, t) => {
  if (v === t) return err(`occurs check failed in kind: ${showKind(v)} in ${showKind(t)}`);
  if (t.tag === 'KFun') {
    occurs(v, t.left);
    occurs(v, t.right);
  }
};

const bind = (v, t) => {
  occurs(v, t);
  v.kind = t;
};

const unifyKinds = (a_, b_) => {
  const a = pruneKind(a_);
  const b = pruneKind(b_);
  if (a === b) return;
  if (a.tag === 'KMeta') return bind(a, b);
  if (b.tag === 'KMeta') return bind(b, a);
  if (a.tag === 'KFun' && b.tag === 'KFun') {
    unifyKinds(a.left, b.left);
    return unifyKinds(a.right, b.right);
  }
  return err(`failed to unify kinds: ${showKind(a)} ~ ${showKind(b)}`);
};

module.exports = {
  unifyKinds,
};
