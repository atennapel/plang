// names
type Name = string;
let id = 0;
const nextId = () => id++;
const fresh = (name: Name = '_') => `${name.split('$')[0]}$${nextId()}`;

// types
type Type = TVar | TMeta | TFun;

interface TVar { tag: 'TVar'; name: Name }
const TVar = (name: Name): TVar => ({ tag: 'TVar', name });

interface TMeta { tag: 'TMeta'; name: Name; type: Type | null }
const TMeta = (name: Name, type?: Type): TMeta => ({ tag: 'TMeta', name, type: type || null });
const freshMeta = (name?: Name) => TMeta(fresh(name));

interface TFun { tag: 'TFun'; left: Type; right: Type }
const TFun = (left: Type, right: Type): TFun => ({ tag: 'TFun', left, right });

const showTy = (ty: Type): string => {
  switch (ty.tag) {
    case 'TVar': return ty.name;
    case 'TMeta': return `^${ty.name}`;
    case 'TFun': return `(${showTy(ty.left)} -> ${showTy(ty.right)})`;
  }
};

type FreeMeta = { [key: string]: boolean };
const freeTy = (ty: Type, free: FreeMeta = {}): FreeMeta => {
  switch (ty.tag) {
    case 'TVar': return free;
    case 'TMeta': free[ty.name] = true; return free;
    case 'TFun': return freeTy(ty.right, freeTy(ty.left, free));
  }
};

// forall
interface Forall { tag: 'Forall'; args: Name[]; type: Type }
const Forall = (args: Name[], type: Type): Forall => ({ tag: 'Forall', args, type });

const showForall = (ty: Forall) =>
  ty.args.length === 0 ? showTy(ty.type) : `forall ${ty.args.join(' ')}. ${showTy(ty.type)}`;

const freeForall = (ty: Forall, free: FreeMeta = {}): FreeMeta => {
  const fr = freeTy(ty.type, free);
  for (let i = 0; i < ty.args.length; i++) fr[ty.args[i]] = false;
  return fr;
};

// exprs
type Expr = Var | Abs | App | Let;

interface Var { tag: 'Var'; name: Name }
const Var = (name: Name): Var => ({ tag: 'Var', name });

interface Abs { tag: 'Abs'; arg: Name; body: Expr }
const Abs = (arg: Name, body: Expr): Abs => ({ tag: 'Abs', arg, body });

interface App { tag: 'App'; left: Expr; right: Expr }
const App = (left: Expr, right: Expr): App => ({ tag: 'App', left, right });

interface Let { tag: 'Let'; name: Name; val: Expr; body: Expr }
const Let = (name: Name, val: Expr, body: Expr): Let => ({ tag: 'Let', name, val, body });

const showExpr = (expr: Expr): string => {
  switch (expr.tag) {
    case 'Var': return expr.name;
    case 'Abs': return `(\\${expr.arg} -> ${showExpr(expr.body)})`;
    case 'App': return `(${showExpr(expr.left)} ${showExpr(expr.right)})`;
    case 'Let': return `(let ${expr.name} = ${showExpr(expr.val)} in ${showExpr(expr.body)})`;
  }
};

// error
const isError = <T>(val: string | T): val is string => typeof val === 'string';

// unification
const prune = (ty: Type): Type => {
  switch (ty.tag) {
    case 'TVar': return ty;
    case 'TMeta': return ty.type ? ty.type = prune(ty.type) : ty;
    case 'TFun': return TFun(prune(ty.left), prune(ty.right));
  }
};
const occurs = (a: Name, ty: Type): boolean => {
  switch (ty.tag) {
    case 'TVar': return false;
    case 'TMeta': return ty.name === a;
    case 'TFun': return occurs(a, ty.left) || occurs(a, ty.right);
  }
};
const bind = (a: TMeta, b: Type): string | null => {
  if (b.tag === 'TMeta' && a.name === b.name) return null;
  if (occurs(a.name, b)) return `${a.name} occurs in ${showTy(b)}`;
  a.type = b;
  return null;
};
const unify_ = (a: Type, b: Type): string | null => {
  if (a.tag === 'TMeta') return bind(a, b);
  if (b.tag === 'TMeta') return bind(b, a);
  if (a.tag === 'TVar' && b.tag === 'TVar' && a.name === b.name) return null;
  if (a.tag === 'TFun' && b.tag === 'TFun') {
    const ret = unify_(a.left, b.left);
    if (isError(ret)) return ret;
    return unify(a.right, b.right);
  }
  return `cannot unify ${showTy(a)} ~ ${showTy(b)}`;
};
const unify = (a: Type, b: Type): string | null => unify_(prune(a), prune(b));

// env
type Env = { [key: string]: Forall };
const extend = (env: Env, name: Name, type: Forall): Env => {
  const nenv: Env = Object.create(env);
  nenv[name] = type;
  return nenv;
};

const freeEnv = (env: Env, free: FreeMeta = {}): FreeMeta => {
  let fr = free;
  for (let k in env) fr = freeForall(env[k], fr);
  return fr;
};

// inference
type InstMap = { [key: string]: TMeta };
const instantiate_ = (ty: Type, map: InstMap): Type => {
  switch (ty.tag) {
    case 'TVar': return ty;
    case 'TMeta': return map[ty.name] || ty;
    case 'TFun': return TFun(instantiate_(ty.left, map), instantiate_(ty.right, map));
  }
};
const instantiate = (ty: Forall): Type => {
  const args = ty.args;
  const map: InstMap = {};
  for (let i = 0; i < args.length; i++) map[args[i]] = freshMeta(args[i]);
  return instantiate_(ty.type, map);
};

const generalize = (env: Env, ty: Type): Forall => {
  const free = freeEnv(env);
  const freeT = freeTy(ty);
  const args: Name[] = [];
  for (let k in freeT) if (!free[k]) args.push(k);
  return Forall(args, ty);
};

const infer_ = (env: Env, expr: Expr): string | Type => {
  switch (expr.tag) {
    case 'Var': return env[expr.name] ? instantiate(env[expr.name]): `undefined var ${expr.name}`;
    case 'Abs':
      const tm = freshMeta(expr.arg);
      const ret = infer(extend(env, expr.arg, Forall([], tm)), expr.body);
      if (isError(ret)) return ret;
      return TFun(tm, ret);
    case 'App':
      const left = infer_(env, expr.left);
      if (isError(left)) return left;
      const right = infer_(env, expr.right);
      if (isError(right)) return right;
      const tmr = freshMeta();
      const ret1 = unify(left, TFun(right, tmr));
      if (isError(ret1)) return ret1;
      return tmr;
    case 'Let':
      const val = infer(env, expr.val);
      if (isError(val)) return val;
      return infer(extend(env, expr.name, generalize(env, val)), expr.body);
  }
  return `cannot infer ${showExpr(expr)}`;
};
const infer = (env: Env, expr: Expr): string | Type => {
  const ret = infer_(env, expr);
  if (isError(ret)) return ret;
  return prune(ret);
};

const inferGen = (env: Env, expr: Expr): string | Forall => {
  const ret = infer(env, expr);
  if (isError(ret)) return ret;
  return generalize({}, ret);
};

// testing
const env: Env = {
  Unit: Forall([], TVar('Unit')),
  True: Forall([], TVar('Bool')),
  id: Forall(['x'], TFun(TMeta('x'), TMeta('x'))),
  k: Forall(['x', 'y'], TFun(TMeta('x'), TFun(TMeta('y'), TMeta('x')))),
};
const ex: Expr = App(Var('k'), Var('True'));
console.log(showExpr(ex));
const ret = inferGen(env, ex);
console.log(isError(ret) ? ret : showForall(ret));

// TODO:
//  - higher-kinded types
