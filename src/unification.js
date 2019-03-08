const {
  showType,
  pruneType,
} = require('./types');

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

const unify = (a_, b_) => {
  const a = pruneType(a_);
  const b = pruneType(b_);
  if (a === b) return;
  if (a.tag === 'TMeta') return bind(a, b);
  if (b.tag === 'TMeta') return bind(b, a);
  if (a.tag === 'TApp' && b.tag === 'TApp') {
    unify(a.left, b.left);
    return unify(a.right, b.right);
  }
  return err(`failed to unify types: ${showType(a)} ~ ${showType(b)}`);
};

module.exports = {
  unify,
};
