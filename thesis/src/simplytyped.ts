import { Result, Ok, Err } from './Result';

type TypeResult<T> = Result<TypeError, T>;
const ok = <T>(t: T): TypeResult<T> => Result.ok(t);
const err = <T>(m: string): TypeResult<T> => Result.err(new TypeError(m));

// types
type Type = TCon | TArr;

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

const typeStr = (type: Type): string => {
  switch(type.tag) {
    case 'TCon':
      return type.val.name;
    case 'TArr':
      return `(${typeStr(type.val.left)} -> ${typeStr(type.val.right)})`;
  }
};
const typeEq = (a: Type, b: Type): boolean => {
  switch(a.tag) {
    case 'TCon':
      return b.tag === 'TCon' && a.val.name === b.val.name;
    case 'TArr':
      return b.tag === 'TArr' &&
        typeEq(a.val.left, b.val.left) &&
        typeEq(a.val.right, b.val.right);
  }
}

// exprs
type Expr = EVar | EAbs | EApp;

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

const exprStr = (expr: Expr): string => {
  switch(expr.tag) {
    case 'EVar': return expr.val.name;
    case 'EAbs':
      return `(\\${expr.val.arg} : ${typeStr(expr.val.type)} -> ${exprStr(expr.val.expr)})`;
    case 'EApp':
      return `(${exprStr(expr.val.left)} ${exprStr(expr.val.right)})`;
  }
}

// context
type ContextElem = CVar;
type Context = ContextElem[];
const emptyContext: Context = [];

interface CVar { tag: 'CVar', val: { name: string, type: Type } }
const cvar = (name: string, type: Type): CVar => ({
  tag: 'CVar',
  val: { name, type },
});

const elemStr = (e: ContextElem) => {
  switch(e.tag) {
    case 'CVar': return `${e.val.name} = ${typeStr(e.val.type)}`;
  }
}
const contextStr = (c: Context) => `[${c.map(elemStr).join(', ')}]`;

const extend = (c: Context, e: ContextElem) => c.concat([e]);
const findVar = (c: Context, name: string): TypeResult<Type> => {
  for(let i = c.length - 1; i >= 0; i--)
    if(c[i].tag === 'CVar' && c[i].val.name === name)
      return ok(c[i].val.type);
  return err(`name not found in context: ${name} in ${contextStr(c)}`); 
};

// typecheck
const typecheck = (c: Context, expr: Expr): TypeResult<Type> => {
  switch(expr.tag) {
    case 'EVar':
      return findVar(c, expr.val.name);
    case 'EAbs':
      return typecheck(extend(c, cvar(expr.val.arg, expr.val.type)), expr.val.expr)
        .map(t => tarr(expr.val.type, t));
    case 'EApp':
      return typecheck(c, expr.val.left)
        .then(tleft => {
          if(tleft.tag === 'TArr') {
            return typecheck(c, expr.val.right)
              .then(tright => {
                if(!typeEq(tleft.val.left, tright))
                  return err(`invalid application, type mismatch: ${typeStr(tleft.val.left)} and ${typeStr(tright)} in ${exprStr(expr)}, context: ${contextStr(c)}`);
                return ok(tleft.val.right);
              });
          } else return err(`invalid application, left side is not an arrow type: ${typeStr(tleft)} in ${exprStr(expr)}, context: ${contextStr(c)}`);
        });
  }
}

// test
const V = evar;
const A = eapps;
const L = eabss;

const tbool = tcon('Bool');
const tint = tcon('Int');
const c = [
  cvar('inc', tarrs(tint, tint)),
];
const e = L([['x', tbool]], A(V('inc'), V('x')));
console.log(exprStr(e));
console.log(''+typecheck(c, e).map(typeStr));
