let _id = 0;
const fresh = () => _id++;
const resetId = () => { _id = 0 };

const TCon = name => ({ tag: 'TCon', name });
const TVar = id => ({ tag: 'TVar', id });
const TMeta = id => ({ tag: 'TMeta', id, type: null });
const TApp = (left, right) => ({ tag: 'TApp', left, right });

const freshTMeta = () => TMeta(fresh());

const TFunC = TCon('->');
const TFun = (left, right) => TApp(TApp(TFunC, left), right);

const showType = t => {
  switch (t.tag) {
    case 'TCon': return t.name;
    case 'TVar': return `'${t.id}`;
    case 'TMeta': return `?${t.id}`;
    case 'TApp':
      return t.left.tag === 'TApp' && t.left.left.tag === 'TCon' &&
        !/[a-z]/i.test(t.left.left.name) ?
        `(${showType(t.left.right)} ${t.left.left.name} ${showType(t.right)})` :
        `(${showType(t.left)} ${showType(t.right)})`;
  }
};

const prune = t => {
  if (t.tag === 'TMeta') {
    if (!t.type) return t;
    const ty = prune(t.type);
    t.type = ty;
    return ty;
  }
  if (t.tag === 'TApp') return TApp(prune(t.left), prune(t.right));
  return t;
};

const occurs = (v, t) => {
  if (v === t) return true;
  if (t.tag === 'TApp') return occurs(v, t.left) || occurs(v, t.right);
  return false;
}

module.exports = {
  resetId,

  TCon,
  TVar,
  TMeta,
  TApp,

  TFunC,
  TFun,

  freshTMeta,

  showType,

  prune,
  occurs,
};
