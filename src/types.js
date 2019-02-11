let _id = 0;
const fresh = () => _id++;
const resetId = () => { _id = 0 };

const TCon = name => ({ tag: 'TCon', name });
const TVar = id => ({ tag: 'TVar', id });
const TMeta = id => ({ tag: 'TMeta', id, type: null });
const TApp = (left, right) => ({ tag: 'TApp', left, right });

const tapp = (fn, args) => args.reduce(TApp, fn);

const freshTMeta = () => TMeta(fresh());

const TFunC = TCon('->');
const TFun = (left, right) => TApp(TApp(TFunC, left), right);
const tfuns = a => a.reduceRight((x, y) => TFun(y, x));

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
};
const occursAny = (vs, t) => {
  if (vs.indexOf(t) >= 0) return t;
  if (t.tag === 'TApp') return occursAny(vs, t.left) || occursAny(vs, t.right);
  return null;
};

const freeTVars = (t, a = []) => {
  if (t.tag === 'TVar') {
    if (a.indexOf(t) >= 0) return a;
    a.push(t);
    return a;
  }
  if (t.tag === 'TApp') {
    freeTVars(t.left, a);
    freeTVars(t.right, a);
    return a;
  }
  return a;
};

module.exports = {
  resetId,

  TCon,
  TVar,
  TMeta,
  TApp,

  tapp,

  freshTMeta,

  TFunC,
  TFun,
  tfuns,

  showType,

  prune,
  occurs,
  occursAny,

  freeTVars,
};