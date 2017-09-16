import { Result, Ok, Err } from './Result';

type TypeResult<T> = Result<TypeError, T>;
const ok = <T>(t: T): TypeResult<T> => Result.ok(t);
const err = <T>(m: string): TypeResult<T> => Result.err(new TypeError(m));

// types
type Type = TVar | TCon | TArr | TAll;

interface TVar { tag: 'TVar', val: { name: string } }
const tvar = (name: string): TVar => ({
  tag: 'TVar',
  val: { name },
});

interface TCon { tag: 'TCon', val: { name: string } }
const tcon = (name: string): TCon => ({
  tag: 'TCon',
  val: { name },
});

interface TArr { tag: 'TArr', val: {left: Type, right: Type} }
const tarr = (left: Type, right: Type): TArr => ({
  tag: 'TArr',
  val: { left, right },
});
const tarrs = (...ts: Type[]) => ts.reduceRight((x, y) => tarr(y, x));

interface TAll { tag: 'TAll', val: { arg: string, type: Type } }
const tall = (arg: string, type: Type): TAll => ({
  tag: 'TAll',
  val: { arg, type },
});
const talls = (args: string[], type: Type) =>
  args.reduceRight((e, arg) => tall(arg, e), type);

const typeStr = (type: Type): string => {
  switch(type.tag) {
    case 'TVar':
      return type.val.name;
    case 'TCon':
      return type.val.name;
    case 'TArr':
      return `(${typeStr(type.val.left)} -> ${typeStr(type.val.right)})`;
    case 'TAll':
      return `(forall ${type.val.arg} . ${typeStr(type.val.type)})`;
  }
};
const typeEq = (a: Type, b: Type): boolean => {
  switch(a.tag) {
    case 'TVar':
      return b.tag === 'TVar' && a.val.name === b.val.name;
    case 'TCon':
      return b.tag === 'TCon' && a.val.name === b.val.name;
    case 'TArr':
      return b.tag === 'TArr' &&
        typeEq(a.val.left, b.val.left) &&
        typeEq(a.val.right, b.val.right);
    case 'TAll':
      return b.tag === 'TAll' &&
        a.val.arg === b.val.arg &&
        typeEq(a.val.type, b.val.type);
  }
}
const typeSubst = (a: Type, arg: string, b: Type): Type => {
  switch(a.tag) {
    case 'TVar':
      return arg === a.val.name? b: a;
    case 'TCon':
      return a;
    case 'TArr':
      return tarr(typeSubst(a.val.left, arg, b), typeSubst(a.val.right, arg, b));
    case 'TAll':
      return arg === a.val.arg? a: tall(a.val.arg, typeSubst(a.val.type, arg, b));
  }
}

// exprs
type Expr = EVar | EAbs | EApp | ETAbs | ETApp;

interface EVar { tag: 'EVar', val: { name: string } }
const evar = (name: string): EVar => ({
  tag: 'EVar',
  val: { name },
});

interface EAbs { tag: 'EAbs', val: { arg: string, type: Type, expr: Expr } }
const eabs = (arg: string, type: Type, expr: Expr): EAbs => ({
  tag: 'EAbs',
  val: { arg, type, expr },
});
const eabss = (args: [string, Type][], expr: Expr) =>
  args.reduceRight((e, [arg, type]) => eabs(arg, type, e), expr);

interface EApp { tag: 'EApp', val: { left: Expr, right: Expr } }
const eapp = (left: Expr, right: Expr): EApp => ({
  tag: 'EApp',
  val: { left, right },
});
const eapps = (...es: Expr[]) => es.reduce(eapp);

interface ETAbs { tag: 'ETAbs', val: { arg: string, expr: Expr } }
const etabs = (arg: string, expr: Expr): ETAbs => ({
  tag: 'ETAbs',
  val: { arg, expr },
});
const etabss = (args: string[], expr: Expr) =>
  args.reduceRight((e, arg) => etabs(arg, e), expr);

interface ETApp { tag: 'ETApp', val: { left: Expr, right: Type } }
const etapp = (left: Expr, right: Type): ETApp => ({
  tag: 'ETApp',
  val: { left, right },
});
const etapps = (e: Expr, ...ts: Type[]) => ts.reduce(etapp, e);

const exprStr = (expr: Expr): string => {
  switch(expr.tag) {
    case 'EVar': return expr.val.name;
    case 'EAbs':
      return `(\\${expr.val.arg} : ${typeStr(expr.val.type)} -> ${exprStr(expr.val.expr)})`;
    case 'EApp':
      return `(${exprStr(expr.val.left)} ${exprStr(expr.val.right)})`;
    case 'ETAbs':
      return `(/${expr.val.arg} -> ${exprStr(expr.val.expr)})`;
    case 'ETApp':
      return `(${exprStr(expr.val.left)} [${typeStr(expr.val.right)}])`;
  }
}

// context
type ContextElem = CVar | CTVar | CTCon;
type Context = ContextElem[];
const emptyContext: Context = [];

interface CVar { tag: 'CVar', val: { name: string, type: Type } }
const cvar = (name: string, type: Type): CVar => ({
  tag: 'CVar',
  val: { name, type },
});
interface CTVar { tag: 'CTVar', val: { name: string } }
const ctvar = (name: string): CTVar => ({
  tag: 'CTVar',
  val: { name },
});
interface CTCon { tag: 'CTCon', val: { name: string } }
const ctcon = (name: string): CTCon => ({
  tag: 'CTCon',
  val: { name },
});

const elemStr = (e: ContextElem) => {
  switch(e.tag) {
    case 'CVar': return `${e.val.name} : ${typeStr(e.val.type)}`;
    case 'CTVar': return `${e.val.name}`;
    case 'CTCon': return `${e.val.name}`;
  }
}
const contextStr = (c: Context) => `[${c.map(elemStr).join(', ')}]`;

const extend = (c: Context, e: ContextElem) => c.concat([e]);
const findVar = (c: Context, name: string): TypeResult<Type> => {
  for(let i = c.length - 1; i >= 0; i--) {
    const e = c[i];
    if(e.tag === 'CVar' && e.val.name === name)
      return ok(e.val.type);
  }
  return err(`var not found in context: ${name} in ${contextStr(c)}`); 
};
const findTVar = (c: Context, name: string): TypeResult<Type> => {
  for(let i = c.length - 1; i >= 0; i--) {
    const e = c[i];
    if(e.tag === 'CTVar' && e.val.name === name)
      return ok(tvar(e.val.name));
  }
  return err(`tvar not found in context: ${name} in ${contextStr(c)}`); 
};
const findTCon = (c: Context, name: string): TypeResult<Type> => {
  for(let i = c.length - 1; i >= 0; i--) {
    const e = c[i];
    if(e.tag === 'CTCon' && e.val.name === name)
      return ok(tcon(e.val.name));
  }
  return err(`tcon not found in context: ${name} in ${contextStr(c)}`); 
};
const freshTVar = (c: Context, name: string): TVar => {
  for(let i = c.length - 1; i >= 0; i--) {
    const e = c[i];
    if(e.tag === 'CTVar' && e.val.name === name)
      return freshTVar(c, `${name}'`);
  }
  return tvar(name);
};

// typecheck
const typeEqv = (c: Context, a: Type, b: Type): TypeResult<Type> => {
  if(typeEq(a, b)) return ok(a);
  if(a.tag === 'TAll' && b.tag === 'TAll') {
    const tv = freshTVar(c, a.val.arg);
    return typeEqv(
      extend(c, ctvar(tv.val.name)),
      typeSubst(a.val.type, a.val.arg, tv),
      typeSubst(b.val.type, b.val.arg, tv)
    );
  }
  return err(`type mismatch, ${typeStr(a)} and ${typeStr(b)} in ${contextStr(c)}`);
};

const wellformed = (c: Context, type: Type): TypeResult<Type> => {
  switch(type.tag) {
    case 'TVar':
      return findTVar(c, type.val.name);
    case 'TCon':
      return findTCon(c, type.val.name);
    case 'TArr':
      return wellformed(c, type.val.left)
        .then(tleft => wellformed(c, type.val.right)
        .map(tright => tarr(tleft, tright)));
    case 'TAll':
      return wellformed(extend(c, ctvar(type.val.arg)), type.val.type)
        .map(t => tall(type.val.arg, t));
  }
};

const typecheck = (c: Context, expr: Expr): TypeResult<Type> => {
  switch(expr.tag) {
    case 'EVar':
      return findVar(c, expr.val.name);
    case 'EAbs':
      return wellformed(c, expr.val.type)
        .then(type => typecheck(extend(c, cvar(expr.val.arg, type)), expr.val.expr)
        .map(t => tarr(type, t)));
    case 'EApp':
      return typecheck(c, expr.val.left)
        .then(tleft => {
          if(tleft.tag === 'TArr') {
            return typecheck(c, expr.val.right)
              .then(tright => typeEqv(c, tleft.val.left, tright)
              .then(_ => ok(tleft.val.right)));
          } else return err(`invalid application, left side is not an arrow type: ${typeStr(tleft)} in ${exprStr(expr)}, context: ${contextStr(c)}`);
        });
    case 'ETAbs':
      return typecheck(extend(c, ctvar(expr.val.arg)), expr.val.expr)
        .map(t => tall(expr.val.arg, t));
    case 'ETApp':
      return wellformed(c, expr.val.right)
        .then(type => typecheck(c, expr.val.left)
        .then(tleft => {
          if(tleft.tag === 'TAll') {
            return ok(typeSubst(tleft.val.type, tleft.val.arg, type));
          } else return err(`invalid type application, left side is not an universal type: ${typeStr(tleft)} in ${exprStr(expr)}, context: ${contextStr(c)}`);
        }));
  }
}

// test
const V = evar;
const A = eapps;
const L = eabss;
const M = etabss;
const T = etapps;

const tbool = tcon('Bool');
const tint = tcon('Int');
const c = [
  ctcon('Bool'),
  ctcon('Int'),

  cvar('zero', tint),
  cvar('inc', tarrs(tint, tint)),
  cvar('id', tall('t', tarrs(tvar('t'), tvar('t')))),
];
const e = A(T(V('id'), tall('x', tarr(tvar('x'), tvar('x')))), V('id'));
console.log(exprStr(e));
console.log(''+typecheck(c, e).map(typeStr));
