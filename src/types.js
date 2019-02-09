let _id = 0;
const fresh = () => _id++;
const resetId = () => { _id = 0 };

const TVar = id => ({ tag: 'TVar', id });
const TMeta = id => ({ tag: 'TMeta', id, type: null });
const TFun = (left, right) => ({ tag: 'TFun', left, right });

const freshTMeta = () => TMeta(fresh());

const showType = t => {
  switch (t.tag) {
    case 'TVar': return `'${t.id}`;
    case 'TMeta': return `?${t.id}`;
    case 'TFun': return `(${showType(t.left)} -> ${showType(t.right)})`;
  }
};

const prune = t => {
  if (t.tag === 'TMeta') {
    if (!t.type) return t;
    const ty = prune(t.type);
    t.type = ty;
    return ty;
  }
  if (t.tag === 'TFun') return TFun(prune(t.left), prune(t.right));
  return t;
};

const occurs = (v, t) => {
  if (v === t) return true;
  if (t.tag === 'TFun') return occurs(v, t.left) || occurs(v, t.right);
  return false;
}

module.exports = {
  resetId,

  TVar,
  TMeta,
  TFun,

  freshTMeta,

  showType,

  prune,
  occurs,
};
