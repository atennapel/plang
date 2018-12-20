const err = (msg: string) => { throw new TypeError(msg) };

// levels
type Level = number;
let currentLevel = 0;
const enterLevel = () => currentLevel++;
const leaveLevel = () => currentLevel--;

// names
type Name = string;
type TVarName = number;
let _id: TVarName = 0;
const freshTVar = (): TVar => TVar(_id++, currentLevel, null); 

// types
type Type = TVar | QVar | TFun;

interface TVar {
  readonly tag: 'TVar';
  readonly name: TVarName;
  type: Type | null;
  level: Level;
}
const TVar = (name: TVarName, level: Level, type: Type | null): TVar =>
  ({ tag: 'TVar', name, level, type });
const isTVar = (type: Type): type is TVar => type.tag === 'TVar';

interface QVar {
  readonly tag: 'QVar';
  readonly name: TVarName;
}
const QVar = (name: TVarName): QVar => ({ tag: 'QVar', name });
const isQVar = (type: Type): type is QVar => type.tag === 'QVar';

interface TFun {
  readonly tag: 'TFun';
  readonly left: Type;
  readonly right: Type;
}
const TFun = (left: Type, right: Type): TFun => ({ tag: 'TFun', left, right });
const isTFun = (type: Type): type is TFun => type.tag === 'TFun';

const showType = (type: Type): string => {
  if (isTVar(type)) return type.type ? showType(type.type) : `?${type.name}`;
  if (isQVar(type)) return `'${type.name}`;
  if (isTFun(type)) return `(${showType(type.left)} -> ${showType(type.right)})`;
  return err('unexpected type in showType');
};

// unification
const occurs = (a: TVar, b: Type): void => {
  if (a === b) return err('occurs check failed');
  if (isTVar(b)) {
    if (b.type) return occurs(a, b.type);
    b.level = Math.min(a.level, b.level);
    return;
  }
  if (isTFun(b)) return occurs(a, b.left), occurs(a, b.right);
};

const bind = (a: TVar, b: Type): void => {
  occurs(a, b);
  a.type = b;
};

const unify = (a: Type, b: Type): void => {
  if (a === b) return;
  if (isTVar(a)) return a.type ? unify(a.type, b) : bind(a, b);
  if (isTVar(b)) return b.type ? unify(a, b.type) : bind(b, a);
  if (isTFun(a) && isTFun(b)) return unify(a.left, b.left), unify(a.right, b.right);
  return err(`cannot unify: ${showType(a)} ~ ${showType(b)}`);
};

// exprs
type Expr = Var | App | Abs | Let;

interface Var {
  readonly tag: 'Var';
  readonly name: Name;
}
const Var = (name: Name): Var => ({ tag: 'Var', name });
const isVar = (expr: Expr): expr is Var => expr.tag === 'Var';

interface App {
  readonly tag: 'App';
  readonly left: Expr;
  readonly right: Expr;
}
const App = (left: Expr, right: Expr): App => ({ tag: 'App', left, right });
const isApp = (expr: Expr): expr is App => expr.tag === 'App';

interface Abs {
  readonly tag: 'Abs';
  readonly name: Name;
  readonly body: Expr;
}
const Abs = (name: Name, body: Expr): Abs => ({ tag: 'Abs', name, body });
const isAbs = (expr: Expr): expr is Abs => expr.tag === 'Abs';

interface Let {
  readonly tag: 'Let';
  readonly name: Name;
  readonly val: Expr;
  readonly body: Expr;
}
const Let = (name: Name, val: Expr, body: Expr): Let =>
  ({ tag: 'Let', name, val, body });
const isLet = (expr: Expr): expr is Let => expr.tag === 'Let';

const showExpr = (expr: Expr): string => {
  if (isVar(expr)) return expr.name;
  if (isApp(expr)) return `(${showExpr(expr.left)} ${showExpr(expr.right)})`;
  if (isAbs(expr)) return `(\\${expr.name} -> ${showExpr(expr.body)})`;
  if (isLet(expr))
    return `(let ${expr.name} = ${showExpr(expr.val)} in ${showExpr(expr.body)})`;
  return err('unexpected expr in showExpr');
};

// inference
type Env = { [name: string]: Type };

const findVar = (env: Env, name: Name): Type =>
  env[name] || err(`undefined var ${name}`);

const inst = (type: Type): Type => {
  if (isQVar(type)) return freshTVar(); 
  if (isTFun(type)) return TFun(inst(type.left), inst(type.right));
  return type;
};

const gen = (type: Type): Type => {
  if (isTVar(type)) {
    if (type.type) return gen(type.type);
    if (type.level > currentLevel) return QVar(type.name);
    return type;
  }
  if (isTFun(type)) return TFun(gen(type.left), gen(type.right));
  return type;
};

const inferWithExtend = (env: Env, name: Name, type: Type, expr: Expr): Type => {
  const prev: Type | null = env[name] || null;
  env[name] = type;
  const tr = infer(env, expr);
  if (prev) env[name] = prev;
  return tr;
}

const infer = (env: Env, expr: Expr): Type => {
  if (isVar(expr)) return inst(findVar(env, expr.name));
  if (isAbs(expr)) {
    const tv = freshTVar();
    const tr = inferWithExtend(env, expr.name, tv, expr.body);
    return TFun(tv, tr);
  }
  if (isApp(expr)) {
    const tf = infer(env, expr.left);
    const ta = infer(env, expr.right);
    const tv = freshTVar();
    unify(tf, TFun(ta, tv));
    return tv;
  }
  if (isLet(expr)) {
    enterLevel();
    const tv = gen(infer(env, expr.val));
    leaveLevel();
    return inferWithExtend(env, expr.name, tv, expr.body);
  }
  return err('unexpected expr in infer');
};

// testing
const microtime = require('microtime');
const env: Env = {};
const expr = Abs('x', Let('y', Var('x'), Var('y')));
console.log(showExpr(expr));
let time = microtime.now();
const type = infer(env, expr);
time = microtime.now() - time;
console.log(showType(type));
console.log(`${time}`);
