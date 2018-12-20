const err = (msg: string) => { throw new TypeError(msg) };

// levels
type Level = number;
const genericLevel = 2147483647;
const markedLevel = -1;
let currentLevel = 0;
const resetLevel = () => { currentLevel = 0 };
const enterLevel = () => { currentLevel++ };
const leaveLevel = () => { currentLevel-- };

// names
type Name = string;
type TVarName = number;
let _id: TVarName = 0;
const resetTVarName = () => { _id = 0 };
const freshTVarName = (): TVarName => _id++;

const resetAll = () => {
  resetTVarName();
  resetLevel();
};

// types
type Type = TVar | TFun;

interface TVar {
  readonly tag: 'TVar';
  readonly name: TVarName;
  type: Type | null;
  level: Level;
}
const TVar = (name: TVarName, level: Level, type: Type | null): TVar =>
  ({ tag: 'TVar', name, level, type });
const isTVar = (type: Type): type is TVar => type.tag === 'TVar';

interface TFun {
  readonly tag: 'TFun';
  readonly left: Type;
  readonly right: Type;
  oldLevel: Level;
  newLevel: Level;
}
const TFun = (left: Type, right: Type, oldLevel: Level, newLevel: Level): TFun =>
  ({ tag: 'TFun', left, right, oldLevel, newLevel });
const isTFun = (type: Type): type is TFun => type.tag === 'TFun';

const showType = (type: Type): string => {
  if (isTVar(type)) {
    if (type.type) return showType(type.type);
    if (type.level === genericLevel) return `'${type.name}`;
    return `?${type.name}`;
  }
  if (isTFun(type)) return `(${showType(type.left)} -> ${showType(type.right)})`;
  return err('unexpected type in showType');
};

// unification
const freshTVar = (): TVar => TVar(freshTVarName(), currentLevel, null);
const tfun = (a: Type, b: Type): TFun => TFun(a, b, currentLevel, currentLevel);

const repr = (type: Type): Type => {
  if (isTVar(type) && type.type) {
    const ty = repr(type.type);
    type.type = ty;
    return ty;
  }
  return type;
};

const getLevel = (type: Type): Level => {
  if (isTVar(type)) return type.level;
  if (isTFun(type)) return type.newLevel;
  return err('unexpected type in getLevel');
};

const cycleFree = (type: Type): void => {
  if (isTVar(type)) {
    if (type.type) return cycleFree(type.type);
    return;
  }
  if (isTFun(type)) {
    if (type.newLevel === markedLevel) return err('occurs check failed in cycleFree');
    const level = type.newLevel;
    type.newLevel = markedLevel;
    cycleFree(type.left);
    cycleFree(type.right);
    type.newLevel = level;
    return;
  }
  return err('unexpected type in cycleFree');
};

let levelAdjustments: TFun[] = [];
const resetLevelAdjustments = () => { levelAdjustments = [] };

const updateLevel = (level: Level, type: Type): void => {
  if (isTVar(type)) {
    if (type.type || type.level === genericLevel)
      return err('updateLevel invariant failed (1)');
    if (level < type.level) type.level = level;
    return;
  }
  if (isTFun(type)) {
    if (type.newLevel === genericLevel) return err('updateLevel invariant failed (2)');
    if (type.newLevel === markedLevel) return err('occurs check failed in updateLevel');
    if (level < type.newLevel) {
      if (type.newLevel === type.oldLevel)
        levelAdjustments.push(type);
      type.newLevel = level;
    }
    return;
  }
  return err('updateLevel invariant failed (3)');
};

const bind = (a: TVar, b: Type): void => {
  if (isTVar(b)) {
    if (a.level > b.level) a.type = b;
    else b.type = a;
    return;
  }
  updateLevel(a.level, b);
  a.type = b;
};

const unify = (a_: Type, b_: Type): void => {
  if (a_ === b_) return;
  const a = repr(a_);
  const b = repr(b_);
  if (a === b) return;
  if (isTVar(a)) return bind(a, b);
  if (isTVar(b)) return bind(b, a);
  if (isTFun(a) && isTFun(b)) {
    if (a.newLevel === markedLevel || b.newLevel === markedLevel)
      return err('occurs check failed in unify');
    const minLevel = Math.min(a.newLevel, b.newLevel);
    a.newLevel = markedLevel;
    b.newLevel = markedLevel;
    unifyLevel(minLevel, a.left, b.left);
    unifyLevel(minLevel, a.right, b.right);
    a.newLevel = minLevel;
    b.newLevel = minLevel;
    return;
  }
  return err(`cannot unify: ${showType(repr(a))} ~ ${showType(repr(b))}`);
};

const unifyLevel = (level: Level, a_: Type, b: Type): void => {
  const a = repr(a_);
  updateLevel(level, a);
  unify(a, b);
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

const forceDelayedAdjustmentsLoop = (acc: TFun[], level: Level, type_: Type): void => {
  const type = repr(type_);
  if (isTVar(type)) {
    if (type.type || type.level <= level) return;
    type.level = level;
    return; 
  }
  if (isTFun(type)) {
    if (type.newLevel === markedLevel)
      return err('occurs check failed in forceDelayedAdjustmentsLoop');
    if (type.newLevel > level) type.newLevel = level;
    forceDelayedAdjustmentsOne(acc, type);
  }
};
const forceDelayedAdjustmentsOne = (acc: TFun[], type: TFun): void => {
  if (type.oldLevel <= currentLevel) acc.push(type);
  else if(type.oldLevel !== type.newLevel) {
    const level = type.newLevel;
    type.newLevel = markedLevel;
    forceDelayedAdjustmentsLoop(acc, level, type.left);
    forceDelayedAdjustmentsLoop(acc, level, type.right);
    type.newLevel = level;
    type.oldLevel = level;
  }
};
const forceDelayedAdjustments = (): void => {
  const acc: TFun[] = [];
  for (let i = levelAdjustments.length - 1; i >= 0; i--)
    forceDelayedAdjustmentsOne(acc, levelAdjustments[i]);
  levelAdjustments = acc;
};

const genR = (type_: Type): void => {
  const type = repr(type_);
  if (isTVar(type)) {
    if (type.type) return;
    if (type.level > currentLevel) type.level = genericLevel;
    return;
  }
  if (isTFun(type)) {
    if (type.newLevel <= currentLevel) return;
    const a = repr(type.left);
    const b = repr(type.right);
    genR(a);
    genR(b);
    const l = Math.max(getLevel(a), getLevel(b));
    type.oldLevel = l;
    type.newLevel = l;
    return;
  }
};
const gen = (type: Type): void => {
  forceDelayedAdjustments();
  genR(type);
};

const instR = (type: Type, subst: { [key: string]: TVar }): Type => {
  if (isTVar(type)) {
    if (type.type) return instR(type.type, subst);
    if (type.level !== genericLevel) return type;
    if (subst[type.name]) return subst[type.name];
    const tv = freshTVar();
    subst[type.name] = tv;
    return tv;
  }
  if (isTFun(type)) {
    if (type.newLevel !== genericLevel) return type;
    const a = instR(type.left, subst);
    const b = instR(type.right, subst);
    return tfun(a, b);
  }
  return type;
};
const inst = (type: Type): Type => instR(type, {});

const inferWithExtend = (env: Env, name: Name, type: Type, expr: Expr): Type => {
  const prev: Type | null = env[name] || null;
  env[name] = type;
  const tr = infer(env, expr);
  if (prev) env[name] = prev;
  return tr;
};

const infer = (env: Env, expr: Expr): Type => {
  if (isVar(expr)) return inst(findVar(env, expr.name));
  if (isAbs(expr)) {
    const tv = freshTVar();
    const tr = inferWithExtend(env, expr.name, tv, expr.body);
    return tfun(tv, tr);
  }
  if (isApp(expr)) {
    const tf = infer(env, expr.left);
    const ta = infer(env, expr.right);
    const tv = freshTVar();
    unify(tf, tfun(ta, tv));
    return tv;
  }
  if (isLet(expr)) {
    enterLevel();
    const tv = infer(env, expr.val);
    leaveLevel();
    gen(tv);
    return inferWithExtend(env, expr.name, tv, expr.body);
  }
  return err('unexpected expr in infer');
};

const inferTop = (env: Env, expr: Expr): Type => {
  resetAll();
  resetLevelAdjustments();
  const ty = infer(env, expr);
  cycleFree(ty);
  gen(ty);
  return ty;
};

// testing
const microtime = require('microtime');
const env: Env = {};
const expr = Abs('x', Let('y', Abs('z', Var('z')), App(Var('y'), Var('x'))));
console.log(showExpr(expr));
let time = microtime.now();
const type = inferTop(env, expr);
time = microtime.now() - time;
console.log(showType(type));
console.log(`${time}`);
// 400 - 500
