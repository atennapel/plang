const {
  showType,
  prune,
  occurs,
} = require('./types');

const bind = (v, t) => {
  if (occurs(v, t)) throw new TypeError(`${showType(v)} occurs in ${showType(t)}`);
  v.type = t;
};

const unify = (a_, b_) => {
  if (a_ === b_) return;
  const a = prune(a_);
  const b = prune(b_);
  if (a === b) return;
  if (a.tag === 'TMeta') return bind(a, b);
  if (b.tag === 'TMeta') return bind(b, a);
  if (a.tag === 'TApp' && b.tag === 'TApp') {
    unify(a.left, b.left);
    unify(a.right, b.right);
    return;
  }
  if (a.tag === 'TVar' && b.tag === 'TVar' && a.id === b.id) return;
  if (a.tag === 'TCon' && b.tag === 'TCon' && a.name === b.name) return;
  throw new TypeError(`cannot unify ${showType(a)} ~ ${showType(b)}`);
};

module.exports = {
  unify,
};
