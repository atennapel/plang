import { Result, Ok, Err } from './Result';

type TypeResult<T> = Result<TypeError, T>;
const ok = <T>(t: T): TypeResult<T> => Result.ok(t);
const err = <T>(m: string): TypeResult<T> => Result.err(new TypeError(m));

// kinds
type Kind = KType | KArr;

interface KType { tag: 'KType' }
const ktype: KType = { tag: 'KType' };

interface KArr { tag: 'KArr', val: {left: Kind, right: Kind} }
const karr = (left: Kind, right: Kind): KArr => ({
  tag: 'KArr',
  val: { left, right },
});
const karrs = (...ts: Kind[]) => ts.reduceRight((x, y) => karr(y, x));

const kindStr = (k: Kind): string => {
  switch(k.tag) {
    case 'KType':
      return 'Type';
    case 'KArr':
      return `(${kindStr(k.val.left)} -> ${kindStr(k.val.right)})`;
  }
};
const kindEq = (a: Kind, b: Kind): boolean => {
  switch(a.tag) {
    case 'KType':
      return b.tag === 'KType';
    case 'KArr':
      return b.tag === 'KArr' &&
        kindEq(a.val.left, b.val.left) &&
        kindEq(a.val.right, b.val.right);
  }
}

// types
type Type = TVar | TCon | TArr | TApp | TAll | TAbs;

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

interface TApp { tag: 'TApp', val: {left: Type, right: Type} }
const tapp = (left: Type, right: Type): TApp => ({
  tag: 'TApp',
  val: { left, right },
});
const tapps = (...ts: Type[]) => ts.reduce(tapp);

interface TAll { tag: 'TAll', val: { arg: string, kind: Kind, type: Type } }
const tall = (arg: string, kind: Kind, type: Type): TAll => ({
  tag: 'TAll',
  val: { arg, kind, type },
});
const talls = (args: [string, Kind][], type: Type) =>
  args.reduceRight((e, [arg, k]) => tall(arg, k, e), type);

interface TAbs { tag: 'TAbs', val: { arg: string, kind: Kind, type: Type } }
const tabs = (arg: string, kind: Kind, type: Type): TAbs => ({
  tag: 'TAbs',
  val: { arg, kind, type },
});
const tabss = (args: [string, Kind][], type: Type) =>
  args.reduceRight((e, [arg, k]) => tabs(arg, k, e), type);

const typeStr = (type: Type): string => {
  switch(type.tag) {
    case 'TVar':
      return type.val.name;
    case 'TCon':
      return type.val.name;
    case 'TArr':
      return `(${typeStr(type.val.left)} -> ${typeStr(type.val.right)})`;
    case 'TApp':
      return `(${typeStr(type.val.left)} ${typeStr(type.val.right)})`;
    case 'TAll':
      return `(forall ${type.val.arg} : ${kindStr(type.val.kind)} . ${typeStr(type.val.type)})`;
    case 'TAbs':
      return `(\\${type.val.arg} : ${kindStr(type.val.kind)} -> ${typeStr(type.val.type)})`;
  }
};
const typeEq = (a: Type, b: Type): boolean => {
  if(a === b) return true;
  switch(a.tag) {
    case 'TVar':
      return b.tag === 'TVar' && a.val.name === b.val.name;
    case 'TCon':
      return b.tag === 'TCon' && a.val.name === b.val.name;
    case 'TArr':
      return b.tag === 'TArr' &&
        typeEq(a.val.left, b.val.left) &&
        typeEq(a.val.right, b.val.right);
    case 'TApp':
      return b.tag === 'TApp' &&
        typeEq(a.val.left, b.val.left) &&
        typeEq(a.val.right, b.val.right);
    case 'TAll':
      return b.tag === 'TAll' &&
        a.val.arg === b.val.arg &&
        kindEq(a.val.kind, b.val.kind) &&
        typeEq(a.val.type, b.val.type);
    case 'TAbs':
      return b.tag === 'TAbs' &&
        a.val.arg === b.val.arg &&
        kindEq(a.val.kind, b.val.kind) &&
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
    case 'TApp':
      return tapp(typeSubst(a.val.left, arg, b), typeSubst(a.val.right, arg, b));
    case 'TAll':
      return arg === a.val.arg? a: tall(a.val.arg, a.val.kind, typeSubst(a.val.type, arg, b));
    case 'TAbs':
      return arg === a.val.arg? a: tabs(a.val.arg, a.val.kind, typeSubst(a.val.type, arg, b));
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

interface ETAbs { tag: 'ETAbs', val: { arg: string, kind: Kind, expr: Expr } }
const etabs = (arg: string, kind: Kind, expr: Expr): ETAbs => ({
  tag: 'ETAbs',
  val: { arg, kind, expr },
});
const etabss = (args: [string, Kind][], expr: Expr) =>
  args.reduceRight((e, [arg, k]) => etabs(arg, k, e), expr);

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
      return `(/${expr.val.arg} : ${kindStr(expr.val.kind)} -> ${exprStr(expr.val.expr)})`;
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
interface CTVar { tag: 'CTVar', val: { name: string, kind: Kind } }
const ctvar = (name: string, kind: Kind): CTVar => ({
  tag: 'CTVar',
  val: { name, kind },
});
interface CTCon { tag: 'CTCon', val: { name: string, kind: Kind } }
const ctcon = (name: string, kind: Kind): CTCon => ({
  tag: 'CTCon',
  val: { name, kind },
});

const elemStr = (e: ContextElem) => {
  switch(e.tag) {
    case 'CVar': return `${e.val.name} : ${typeStr(e.val.type)}`;
    case 'CTVar': return `${e.val.name} :k ${kindStr(e.val.kind)}`;
    case 'CTCon': return `${e.val.name} :k ${kindStr(e.val.kind)}`;
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
const findTVar = (c: Context, name: string): TypeResult<Kind> => {
  for(let i = c.length - 1; i >= 0; i--) {
    const e = c[i];
    if(e.tag === 'CTVar' && e.val.name === name)
      return ok(e.val.kind);
  }
  return err(`tvar not found in context: ${name} in ${contextStr(c)}`); 
};
const findTCon = (c: Context, name: string): TypeResult<Kind> => {
  for(let i = c.length - 1; i >= 0; i--) {
    const e = c[i];
    if(e.tag === 'CTCon' && e.val.name === name)
      return ok(e.val.kind);
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
const kindcheck = (c: Context, t: Type): TypeResult<Kind> => {
  switch(t.tag) {
    case 'TVar':
      return findTVar(c, t.val.name);
    case 'TCon':
      return findTCon(c, t.val.name);
    case 'TArr':
      return kindcheck(c, t.val.left)
        .then(kleft => kindcheck(c, t.val.right)
        .then(kright => {
          if(!kindEq(kleft, ktype)) return err(`kind needs to be Type in arrow (left): ${t}`);
          if(!kindEq(kright, ktype)) return err(`kind needs to be Type in arrow (right): ${t}`);
          return ok(ktype);
        }));
    case 'TApp':
      return kindcheck(c, t.val.left)
        .then(kleft => kindcheck(c, t.val.right)
        .then(kright => {
          if(kleft.tag === 'KArr') {
            if(!kindEq(kleft.val.left, kright))
              return err(`invalid right side in type application: ${kindStr(kleft.val.left)} and ${kindStr(kright)} in ${typeStr(t)} in ${contextStr(c)}`);
            return ok(kleft.val.right);
          } else return err(`invalid left side in type application: ${kindStr(kleft)} in ${typeStr(t)} in ${contextStr(c)}`);
        }));
    case 'TAll':
      return kindcheck(extend(c, ctvar(t.val.arg, t.val.kind)), t.val.type);
    case 'TAbs':
      return kindcheck(extend(c, ctvar(t.val.arg, t.val.kind)), t.val.type)
        .map(kbody => karr(t.val.kind, kbody));
  }
};

const typeRename = (c: Context, t: Type): TypeResult<Type> => {
  switch(t.tag) {
    case 'TArr':
      return typeRename(c, t.val.left)
        .then(left => typeRename(c, t.val.right)
        .map(right => tarr(left, right)));
    case 'TApp':
      return typeRename(c, t.val.left)
        .then(left => typeRename(c, t.val.right)
        .map(right => tapp(left, right)));
    case 'TAll':
      const tv = freshTVar(c, t.val.arg);
      return typeRename(extend(c, ctvar(tv.val.name, t.val.kind)), typeSubst(t.val.type, t.val.arg, tv))
        .map(body => tall(tv.val.name, t.val.kind, body));
    case 'TAbs':
      const tv2 = freshTVar(c, t.val.arg);
      return typeRename(extend(c, ctvar(tv2.val.name, t.val.kind)), typeSubst(t.val.type, t.val.arg, tv2))
        .map(body => tabs(tv2.val.name, t.val.kind, body));
  }
  return ok(t);
}

const typeReduce = (c: Context, t: Type): TypeResult<Type> => {
  switch(t.tag) {
    case 'TArr':
      return typeReduce(c, t.val.left)
        .then(left => typeReduce(c, t.val.right)
        .map(right => tarr(left, right)));
    case 'TApp':
      return typeReduce(c, t.val.left)
        .then(left => typeReduce(c, t.val.right)
        .then(right => {
          if(left.tag === 'TAbs') {
            return typeRename(c, right).then(right => typeReduce(c, typeSubst(left.val.type, left.val.arg, right)))
          } else return ok(tapp(left, right));
        }));
    case 'TAll':
      return typeReduce(extend(c, ctvar(t.val.arg, t.val.kind)), t.val.type)
        .map(tb => tall(t.val.arg, t.val.kind, tb));
    case 'TAbs':
      return typeReduce(extend(c, ctvar(t.val.arg, t.val.kind)), t.val.type)
        .map(tb => tabs(t.val.arg, t.val.kind, tb));
  }
  return ok(t);
};

const typeEqv = (c: Context, a: Type, b: Type): TypeResult<Type> => {
  return wellformed(c, a)
    .then(a => wellformed(c, b)
    .then(b => kindcheck(c, a)
    .then(ka => kindcheck(c, b)
    .then(kb => {
      if(!kindEq(ka, kb))
        return err(`kind mismatch: ${kindStr(ka)} and ${kindStr(kb)} in ${typeStr(a)} and ${typeStr(b)} in ${contextStr(c)}`);
      if(typeEq(a, b)) return ok(a);
      return typeReduce(c, a)
        .then(a => typeReduce(c, b)
        .then(b => {
          if(typeEq(a, b)) return ok(a);
          switch(a.tag) {
            case 'TArr':
              if(b.tag === 'TArr') {
                return typeEqv(c, a.val.left, b.val.right)
                  .then(tleft => typeEqv(c, a.val.right, b.val.right)
                  .map(tright => tarr(tleft, tright)));
              }
              return err(`type mismatch, ${typeStr(a)} and ${typeStr(b)} in ${contextStr(c)}`);
            case 'TApp':
              if(b.tag === 'TApp') {
                return typeEqv(c, a.val.left, b.val.right)
                  .then(tleft => typeEqv(c, a.val.right, b.val.right)
                  .map(tright => tapp(tleft, tright)));
              }
              return err(`type mismatch, ${typeStr(a)} and ${typeStr(b)} in ${contextStr(c)}`);
            case 'TAll':
              if(b.tag === 'TAll') {
                const tv = freshTVar(c, a.val.arg);
                return typeEqv(
                  extend(c, ctvar(tv.val.name, a.val.kind)),
                  typeSubst(a.val.type, a.val.arg, tv),
                  typeSubst(b.val.type, b.val.arg, tv)
                );
              }
            case 'TAbs':
              if(b.tag === 'TAbs') {
                const tv = freshTVar(c, a.val.arg);
                return typeEqv(
                  extend(c, ctvar(tv.val.name, a.val.kind)),
                  typeSubst(a.val.type, a.val.arg, tv),
                  typeSubst(b.val.type, b.val.arg, tv)
                );
              }
              return err(`type mismatch, ${typeStr(a)} and ${typeStr(b)} in ${contextStr(c)}`);
          }
          return err(`type mismatch, ${typeStr(a)} and ${typeStr(b)} in ${contextStr(c)}`);
        }));
    }))));
};

const wellformed = (c: Context, type: Type): TypeResult<Type> => {
  switch(type.tag) {
    case 'TVar':
      return findTVar(c, type.val.name).map(_ => type);
    case 'TCon':
      return findTCon(c, type.val.name).map(_ => type);
    case 'TArr':
      return wellformed(c, type.val.left)
        .then(tleft => wellformed(c, type.val.right)
        .map(tright => tarr(tleft, tright)));
    case 'TApp':
      return wellformed(c, type.val.left)
        .then(tleft => wellformed(c, type.val.right)
        .map(tright => tapp(tleft, tright)));
    case 'TAll':
      return wellformed(extend(c, ctvar(type.val.arg, type.val.kind)), type.val.type)
        .map(t => tall(type.val.arg, type.val.kind, t));
    case 'TAbs':
      return wellformed(extend(c, ctvar(type.val.arg, type.val.kind)), type.val.type)
        .map(t => tabs(type.val.arg, type.val.kind, t));
  }
};

const typecheck = (c: Context, expr: Expr): TypeResult<Type> => {
  switch(expr.tag) {
    case 'EVar':
      return findVar(c, expr.val.name)
        .then(type => kindcheck(c, type)
        .then(k => {
          if(!kindEq(k, ktype))
            return err(`invalid kind of variable: ${kindStr(k)} in ${exprStr(expr)} in ${contextStr(c)}`);
          return typeReduce(c, type);
        }));
    case 'EAbs':
      return wellformed(c, expr.val.type)
        .then(type => kindcheck(c, type)
        .then(k => {
          if(!kindEq(k, ktype))
            return err(`invalid kind in lambda: ${kindStr(k)} in ${exprStr(expr)} in ${contextStr(c)}`);
          return typecheck(extend(c, cvar(expr.val.arg, type)), expr.val.expr)
            .then(t => typeReduce(c, tarr(type, t)));
        }));
    case 'EApp':
      return typecheck(c, expr.val.left)
        .then(tleft => {
          if(tleft.tag === 'TArr') {
            return typecheck(c, expr.val.right)
              .then(tright => typeEqv(c, tleft.val.left, tright)
              .then(_ => typeReduce(c, tleft.val.right)));
          } else return err(`invalid application, left side is not an arrow type: ${typeStr(tleft)} in ${exprStr(expr)}, context: ${contextStr(c)}`);
        });
    case 'ETAbs':
      return typecheck(extend(c, ctvar(expr.val.arg, expr.val.kind)), expr.val.expr)
        .then(t => typeReduce(c, tall(expr.val.arg, expr.val.kind, t)));
    case 'ETApp':
      return wellformed(c, expr.val.right)
        .then(type => typecheck(c, expr.val.left)
        .then(tleft => {
          if(tleft.tag === 'TAll') {
            return kindcheck(c, type)
              .then(ka => {
                if(!kindEq(ka, tleft.val.kind))
                  return err(`invalid type application, kind mismatch: ${kindStr(ka)} and ${kindStr(tleft.val.kind)} in ${exprStr(expr)}, context: ${contextStr(c)}`);
                return typeReduce(c, typeSubst(tleft.val.type, tleft.val.arg, type));
              });
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
const tlist = tcon('List');
const tpair = tcon('Pair');
const c = [
  ctcon('Bool', ktype),
  ctcon('Int', ktype),
  ctcon('List', karr(ktype, ktype)),
  ctcon('Pair', karrs(ktype, ktype, ktype)),

  cvar('zero', tint),
  cvar('inc', tarrs(tint, tint)),
  cvar('id', tall('t', ktype, tarrs(tvar('t'), tvar('t')))),
];
const e = T(M([['x', karr(karr(ktype, ktype), ktype)]], L([['x', tapp(tvar('x'), tabs('x', ktype, tvar('x')))]], V('x'))), tabs('x', karr(ktype, ktype), tapp(tvar('x'), tint)));
console.log(exprStr(e));
console.log(''+typecheck(c, e).map(typeStr));
