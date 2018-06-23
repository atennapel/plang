import { Result, Ok, Err, isOk, isErr } from './Result';
import { impossible } from './util'; 

import {
  Type,
  TCon,
  TVar,
  TEx,
  TFun,
  TForall,
  tcon,
  tex,
  tfun,
  tvar,
  tforalls,
  TApp,
  tapp,
  tforall,
  tfuns,
} from './types';
import {
  Context,
  CKCon,
  CTCon,
  CTVar,
  CTEx,
  CVar,
  CSolved,
  CMarker,
  ctex,
  isCTEx,
  cmarker,
  isCMarker,
  ctvar,
  isCTVar,
  csolved,
  cvar,
  isCVar,
  ctcon,
  isCTCon,
  ckcon,
  isCKCon,
} from './context';
import {
  Expr,
  EVar,
  EAbs,
  EApp,
  ETApp,
  ETAbs,
  EAnno,
  evar,
  eanno,
} from './exprs';
import {
  Kind,
  KCon,
  KFun,
  kcon,
  kfun,
} from './kinds';
import {
  Definition,
  DValue,
  DData,
} from './definitions';

// errors
type IResult<T> = Result<TypeError, T>;
const err = <T>(msg: string): IResult<T> => Result.err(new TypeError(msg));
const ok = <T>(val: T): IResult<T> => Result.ok(val);
const not = <T>(r: IResult<T>, m: string): IResult<null> => {
  if(isOk(r)) return err(m);
  if(isErr(r)) return ok(null);
  return impossible();
};
const check = (b: boolean, m: string): IResult<null> => b? ok(null): err(m);

function noDups(d: string[]): IResult<null> {
  const o: { [key: string]: boolean } = {};
  for(let i = 0; i < d.length; i++) {
    if(o[d[i]]) return err(`duplicate ${d[i]}`);
    o[d[i]] = true;
  }
  return ok(null);
}

function mapM<A, B>(a: A[], fn: (a: A) => IResult<B>): IResult<B[]> {
  const l = a.length;
  const r: B[] = [];
  for(let i = 0; i < l; i++) {
    const c = fn(a[i]);
    if(isErr(c)) return new Err(c.err);
    else if(isOk(c)) r.push(c.val);
    else return impossible();
  }
  return ok(r);
};

// fresh
const fresh = (ns: string[], n: string): string => {
  const l = ns.length;
  for(let i = 0; i < l; i++) {
    if(ns[i] === n) {
      const m = n.split('').reverse().join('').match(/^([0-9]+).+$/i);
      if(!m) {
        return fresh(ns, `${n}0`);
      } else {
        const jr = m[1];
        const jl = jr.length;
        const j = +(jr.split('').reverse().join(''));
        return fresh(ns, `${n.slice(0, -jl)}${j+1}`);
      }
    }
  }
  return n;
};

// util
const findVar = (ctx: Context, name: string): IResult<Type> => {
  const r = ctx.findVar(name);
  return r === null? err(`var ${name} not found in ${ctx}`): ok(r);
};
const findSolved = (ctx: Context, name: string): IResult<Type> => {
  const r = ctx.findSolved(name);
  return r === null? err(`var ${name} not found in ${ctx}`): ok(r);
};
const findKCon = (ctx: Context, name: string): IResult<null> => {
  return ctx.findKCon(name) === null? err(`tcon ${name} not found in ${ctx}`): ok(null);
};
const findTCon = (ctx: Context, name: string): IResult<Kind> => {
  const r = ctx.findTCon(name);
  return r === null? err(`tcon ${name} not found in ${ctx}`): ok(r);
};
const findTVar = (ctx: Context, name: string): IResult<Kind> => {
  const r = ctx.findTVar(name);
  return r === null? err(`tvar ${name} not found in ${ctx}`): ok(r);
};
const findEx = (ctx: Context, name: string): IResult<Kind> => {
  const r = ctx.findEx(name);
  return r === null? err(`ex ^${name} not found in ${ctx}`): ok(r);
};
const findMarker = (ctx: Context, name: string): IResult<null> => {
  return ctx.findMarker(name) === null? err(`marker |>${name} not found in ${ctx}`): ok(null);
};
const findExOrSolved = (ctx: Context, name: string): IResult<Kind> => {
  const r = ctx.findExOrSolved(name);
  return r === null? err(`ex or solved ^${name} not found in ${ctx}`): ok(r);
};

const orderedTExs = (texs: [string, Kind][], ty: Type): [string, Kind][] => {
  const o = ty.texs();
  const r: string[] = [];
  const texsn = texs.map(([n, _]) => n);
  for(let i = 0; i < o.length; i++) {
    const c = o[i];
    if(texsn.indexOf(c) >= 0 && r.indexOf(c) < 0)
      r.push(c);
  }
  return r.map(n => texs[texsn.indexOf(n)]);
};

// initial context
export const ktype = kcon('Type');
export const initialContext = new Context([
  ckcon('Type'),
]);

// wf
function checkKindType(kind: Kind): IResult<null> {
  return ktype.equals(kind)? ok(null): err(`kind is not ${ktype}: ${kind}`);
}

function kindWF(ctx: Context, kind: Kind): IResult<null> {
  // console.log(`kindWF ${kind} in ${ctx}`);
  if(kind instanceof KCon) return findKCon(ctx, kind.name);
  if(kind instanceof KFun)
    return kindWF(ctx, kind.left).then(() => kindWF(ctx, kind.right));
  return impossible();
}

function typeWF(ctx: Context, ty: Type): IResult<Kind> {
  //console.log(`typeWF ${ty} in ${ctx}`);
  if(ty instanceof TCon) return findTCon(ctx, ty.name).then(k => kindWF(ctx, k).map(() => k));
  if(ty instanceof TVar) return findTVar(ctx, ty.name).then(k => kindWF(ctx, k).map(() => k));
  if(ty instanceof TEx) return findExOrSolved(ctx, ty.name).then(k => kindWF(ctx, k).map(() => k));
  if(ty instanceof TFun)
    return typeWF(ctx, ty.left).then(k1 =>
        checkKindType(k1).then(() => typeWF(ctx, ty.right).then(k2 => checkKindType(k2).map(() => ktype))));
  if(ty instanceof TForall) return kindWF(ctx, ty.kind).then(() => typeWF(ctx.add(ctvar(ty.name, ty.kind)), ty.type));
  if(ty instanceof TApp)
    return typeWF(ctx, ty.left).then(kleft => {
      if(kleft instanceof KFun)
        return typeWF(ctx, ty.right)
          .then(kright => {
            if(!kright.equals(kleft.left))
              return err(`kind mismatch in type constructor: ${ty} in ${ctx}`);
            return ok(kleft.right);
          });
      else return err(`not a type constructor: ${ty} in ${ctx}`);
    });
  return impossible();
}

function contextWF(ctx: Context): IResult<null> {
  // console.log(`contextWF ${ctx}`);
  const a = ctx.elems;
  const l = a.length;
  for(let i = 0; i < l; i++) {
    const e = a[i];
    const p = new Context(a.slice(0, i));
    if(e instanceof CKCon) {
      const m = not(findKCon(p, e.name), `duplicate kcon ^${e.name}`);
      if(isErr(m)) return m;
    } else if(e instanceof CTCon) {
      const m = not(findTCon(p, e.name), `duplicate tcon ${e.name}`)
        .then(() => kindWF(p, e.kind));
      if(isErr(m)) return m;
    } else if(e instanceof CTVar) {
      const m = not(findTVar(p, e.name), `duplicate tvar ${e.name}`)
        .then(() => kindWF(p, e.kind));
      if(isErr(m)) return m;
    } else if(e instanceof CTEx || e instanceof CSolved) {
      const m = not(findExOrSolved(p, e.name), `duplicate tex ^${e.name}`)
        .then(() => kindWF(p, e.kind));
      if(isErr(m)) return m;
    } else if(e instanceof CVar) {
      const m = not(findVar(p, e.name), `duplicate var ${e.name}`)
        .then(() => typeWF(p, e.type).then(k => checkKindType(k)));
      if(isErr(m)) return m;
    } else if(e instanceof CMarker) {
      const m = not(findMarker(p, e.name), `duplicate marker ^${e.name}`)
        .then(() => not(findExOrSolved(p, e.name), `duplicate marker ^${e.name}`));
      if(isErr(m)) return m;
    } else return impossible();
  }
  return ok(null);
}

// subtype
function subtype(ctx: Context, a: Type, b: Type): IResult<Context> {
  // console.log(`subtype ${a} and ${b} in ${ctx}`);
  const wf = typeWF(ctx, a).then(k1 => typeWF(ctx, b).then(k2 => ok({k1, k2})));
  if(isErr(wf)) return new Err(wf.err);
  const wfok = wf as Ok<TypeError, { k1: Kind, k2: Kind }>;
  const k = wfok.val.k1;
  if(!k.equals(wfok.val.k2))
    return err(`kind mismatch ${a} and ${b}, ${k} and ${wfok.val.k2} in ${ctx}`);
  if(((a instanceof TVar && b instanceof TVar) ||
    (a instanceof TEx && b instanceof TEx) ||
    (a instanceof TCon && b instanceof TCon)) && a.name === b.name)
    return ok(ctx);
  if(a instanceof TApp && b instanceof TApp)
    return subtype(ctx, a.left, b.left)
      .then(ctx_ => subtype(ctx_, ctx_.apply(a.right), ctx_.apply(b.right)));
  if(a instanceof TFun && b instanceof TFun)
    return subtype(ctx, b.left, a.left)
      .then(ctx_ => subtype(ctx_, ctx_.apply(a.right), ctx_.apply(b.right)));
  if(a instanceof TForall) {
    const x = fresh(ctx.texs(), a.name);
    return subtype(ctx.add(cmarker(x), ctex(x, a.kind)), a.open(tex(x)), b)
      .then(ctx_ => ok(ctx_.split(isCMarker(x)).left));
  }
  if(b instanceof TForall) {
    const x = fresh(ctx.tvars(), b.name);
    return subtype(ctx.add(ctvar(x, b.kind)), a, b.open(tvar(x)))
      .then(ctx_ => ok(ctx_.split(isCTVar(x)).left));
  }
  if(a instanceof TEx)
    return findEx(ctx, a.name)
      .then(() => check(!b.containsEx(a.name), `occurs check failed L: ${a} in ${b}`))
      .then(() => instL(ctx, a.name, b));
  if(b instanceof TEx)
    return findEx(ctx, b.name)
      .then(() => check(!a.containsEx(b.name), `occurs check failed R: ${b} in ${a}`))
      .then(() => instR(ctx, a, b.name));
  return err(`subtype failed: ${a} <: ${b} in ${ctx}`);
}

// inst
function solve(ctx: Context, name: string, ty: Type): IResult<Context> {
  // console.log(`solve ${name} and ${ty} in ${ctx}`);
  if(ty.isMono()) {
    const s = ctx.split(isCTEx(name));
    return typeWF(s.left, ty)
      .then(k => ok(s.left.add(csolved(name, k, ty)).append(s.right)));
  } else return err(`polymorphic type in solve: ${name} := ${ty} in ${ctx}`);
}

function instL(ctx: Context, a: string, b: Type): IResult<Context> {
  // console.log(`instL ${a} and ${b} in ${ctx}`);
  if(b instanceof TEx && ctx.isOrdered(a, b.name)) return solve(ctx, b.name, tex(a));
  if(b instanceof TEx && ctx.isOrdered(b.name, a)) return solve(ctx, a, b);
  const r = solve(ctx, a, b);
  if(isOk(r)) return r;
  if(b instanceof TApp) {
    return typeWF(ctx, b.left).then((kf: KFun) => {
      const texs = ctx.texs();
      const a1 = fresh(texs, a);
      const a2 = fresh(texs.concat([a1]), a);
      return instL(ctx.replace(isCTEx(a), new Context([ctex(a2, kf.left), ctex(a1, kf), csolved(a, kf.right, tapp(tex(a1), tex(a2)))])), a1, b.left)
        .then(ctx_ => instL(ctx_, a2, ctx_.apply(b.right)));
    });
  }
  if(b instanceof TFun) {
    const texs = ctx.texs();
    const a1 = fresh(texs, a);
    const a2 = fresh(texs.concat([a1]), a);
    return instR(ctx.replace(isCTEx(a), new Context([ctex(a2, ktype), ctex(a1, ktype), csolved(a, ktype, tfun(tex(a1), tex(a2)))])), b.left, a1)
      .then(ctx_ => instL(ctx_, a2, ctx_.apply(b.right)));
  }
  if(b instanceof TForall) {
    const x = fresh(ctx.tvars(), b.name);
    return instL(ctx.add(ctvar(x, b.kind)), a, b.open(tvar(x)))
      .then(ctx_ => ok(ctx_.split(isCTVar(x)).left));
  }
  return err(`instL failed: ${a} and ${b} in ${ctx}`);
}

function instR(ctx: Context, a: Type, b: string): IResult<Context> {
  // console.log(`instR ${a} and ${b} in ${ctx}`);
  if(a instanceof TEx && ctx.isOrdered(b, a.name)) return solve(ctx, a.name, tex(b));
  if(a instanceof TEx && ctx.isOrdered(a.name, b)) return solve(ctx, b, a);
  const r = solve(ctx, b, a);
  if(isOk(r)) return r;
  if(a instanceof TApp) {
    return typeWF(ctx, a.left).then((kf: KFun) => {
      const texs = ctx.texs();
      const b1 = fresh(texs, b);
      const b2 = fresh(texs.concat([b1]), b);
      return instR(ctx.replace(isCTEx(b), new Context([ctex(b2, kf.left), ctex(b1, kf), csolved(b, kf.right, tapp(tex(b1), tex(b2)))])), a.left, b1)
        .then(ctx_ => instR(ctx_, ctx_.apply(a.right), b2));
    });
  }
  if(a instanceof TFun) {
    const texs = ctx.texs();
    const b1 = fresh(texs, b);
    const b2 = fresh(texs.concat([b1]), b);
    return instL(ctx.replace(isCTEx(b), new Context([ctex(b2, ktype), ctex(b1, ktype), csolved(b, ktype, tfun(tex(b1), tex(b2)))])), b1, a.left)
      .then(ctx_ => instR(ctx_, ctx_.apply(a.right), b2));
  }
  if(a instanceof TForall) {
    const x = fresh(ctx.texs(), a.name);
    return instR(ctx.add(cmarker(x), ctex(x, a.kind)), a.open(tex(x)), b)
      .then(ctx_ => ok(ctx_.split(isCMarker(x)).left));
  }
  return err(`instR failed: ${a} and ${b} in ${ctx}`);
}

// synth/check
function synth(ctx: Context, e: Expr): IResult<{ ctx: Context, ty: Type }> {
  // console.log(`synth ${e} in ${ctx}`);
  const r = contextWF(ctx);
  if(isErr(r)) return new Err(r.err);
  if(e instanceof EVar) return findVar(ctx, e.name).then(ty => ok({ ctx, ty }));
  if(e instanceof EAbs) {
    if(e.isAnnotated()) {
      const ty = e.type as Type;
      return typeWF(ctx, ty).then(k => checkKindType(k).then(() => {
        const x = fresh(ctx.vars(), e.name);
        const b = fresh(ctx.texs(), e.name);
        return checkTy(ctx.add(cmarker(b), ctex(b, ktype), cvar(x, ty)), e.open(evar(x)), tex(b))
          .then(ctx_ => {
            const s = ctx_.split(isCMarker(b));
            const t = s.right.apply(tfun(ty, tex(b)));
            const u = orderedTExs(s.right.unsolved(), t);
            return ok({
              ctx: s.left,
              ty: tforalls(u, u.reduce((t, [n, _]) => t.substEx(n, tvar(n)), t)),
            });
          });
      }));
    } else {
      const x = fresh(ctx.vars(), e.name);
      const texs = ctx.texs();
      const a = fresh(texs, e.name);
      const b = fresh(texs.concat([a]), e.name);
      return checkTy(ctx.add(cmarker(a), ctex(a, ktype), ctex(b, ktype), cvar(x, tex(a))), e.open(evar(x)), tex(b))
        .then(ctx_ => {
          const s = ctx_.split(isCMarker(a));
          const t = s.right.apply(tfun(tex(a), tex(b)));
          const u = orderedTExs(s.right.unsolved(), t);
          return ok({
            ctx: s.left,
            ty: tforalls(u, u.reduce((t, [n, _]) => t.substEx(n, tvar(n)), t)),
          });
        });
    }
  }
  if(e instanceof EApp)
    return synth(ctx, e.left)
      .then(({ ctx: ctx_, ty}) => synthapp(ctx_, ctx_.apply(ty), e.right));
  if(e instanceof EAnno)
    return typeWF(ctx, e.type)
      .then(() => checkTy(ctx, e.expr, e.type).then(ctx_ => ok({ ctx: ctx_, ty: e.type })));
  if(e instanceof ETAbs)
    return kindWF(ctx, e.kind).then(() => {
      const x = fresh(ctx.vars(), e.name);
      return synth(ctx.add(ctvar(x, e.kind)), e.expr.substType(e.name, tvar(x))).then(({ctx: ctx_, ty}) =>
        ok({ ctx: ctx_, ty: tforall(x, e.kind, ctx_.apply(ty)) }));
    });
  if(e instanceof ETApp)
    return typeWF(ctx, e.type)
      .then(() => synth(ctx, e.expr)
      .then(({ ctx: ctx_, ty }) => {
        const ty_ = ctx_.apply(ty);
        return ty_ instanceof TForall? ok({ ctx: ctx_, ty: ty_.open(e.type) }):
          err(`type application on non-polymorphic type: ${e} with ${ty_} in ${ctx_}`);
      }));
  return err(`cannot synth ${e} in ${ctx}`);
}

function checkTy(ctx: Context, e: Expr, ty: Type): IResult<Context> {
  // console.log(`checkTy ${e} and ${ty} in ${ctx}`);
  const r = contextWF(ctx);
  if(isErr(r)) return new Err(r.err);
  if(ty instanceof TForall) {
    const x = fresh(ctx.tvars(), ty.name);
    return checkTy(ctx.add(ctvar(x, ty.kind)), e, ty.open(tvar(x)))
      .then(ctx_ => ok(ctx_.split(isCTVar(x)).left));
  }
  if(e instanceof EAbs && !e.isAnnotated() && ty instanceof TFun) {
    const x = fresh(ctx.vars(), e.name);
    return checkTy(ctx.add(cvar(x, ty.left)), e.open(evar(x)), ty.right)
      .then(ctx_ => ok(ctx_.split(isCVar(x)).left));
  }
  return synth(ctx, e)
    .then(({ ctx: ctx_, ty: ty_ }) => subtype(ctx_, ctx_.apply(ty_), ctx_.apply(ty)));
}

function synthapp(ctx: Context, ty: Type, e: Expr): IResult<{ ctx: Context, ty: Type }> {
  // console.log(`synthapp ${ty} and ${e} in ${ctx}`);
  const r = contextWF(ctx);
  if(isErr(r)) return new Err(r.err);
  if(ty instanceof TForall) {
    const x = fresh(ctx.texs(), ty.name);
    return synthapp(ctx.add(ctex(x, ty.kind)), ty.open(tex(x)), e);
  }
  if(ty instanceof TEx)
    return findEx(ctx, ty.name)
      .then(() => {
        const texs = ctx.texs();
        const a1 = fresh(texs, ty.name);
        const a2 = fresh(texs.concat([a1]), ty.name);
        return checkTy(ctx.replace(
            isCTEx(ty.name),
            new Context([ctex(a2, ktype), ctex(a1, ktype), csolved(ty.name, ktype, tfun(tex(a1), tex(a2)))])),
            e, tex(a1)
          )
          .then(ctx_ => ok({ ctx: ctx_, ty: tex(a2) }));
      });
  if(ty instanceof TFun)
    return checkTy(ctx, e, ty.left)
      .then(ctx_ => ok({ ctx: ctx_, ty: ty.right }));
  return err(`cannot synthapp ${ty} with ${e} in ${ctx}`);
}

export function infer(ctx: Context, e: Expr): IResult<{ ctx: Context, ty: Type }> {
  return synth(ctx, e)
    .then(({ ctx: ctx__, ty }) => contextWF(ctx__)
    .then(() => {
      const ctx_ = ctx__.applyContext(ctx__);
      const ty_ = ctx_.apply(ty);
      return typeWF(ctx_, ty_).then(k => checkKindType(k).then(() => {
        if(ctx_.isComplete()) return ok({ ctx: ctx_, ty: ty_ });
        const unsolved = ctx_.unsolved();
        const unsolvedNames = unsolved.map(([n, _]) => n);
        const u = orderedTExs(unsolved, ty_);
        return ok({
          ctx: ctx_.removeAll(e => (e instanceof CSolved) || ((e instanceof CTEx || e instanceof CMarker) && unsolvedNames.indexOf(e.name) >= 0)),
          ty: tforalls(u, u.reduce((t, [n, _]) => t.substEx(n, tvar(n)), ty_)),
        });
      }))
    }));
}

export function inferDefinition(ctx: Context, d: Definition): IResult<Context> {
  if(d instanceof DValue) {
    console.log(''+d);
    return infer(ctx, (d.type? eanno(d.val, d.type): d.val))
      .map(x => ({ ty: x.ty, ctx: x.ctx.removeAll(e => e instanceof CSolved) }))
      .then(({ ctx, ty }) => contextWF(ctx.add(cvar(d.name, ty)))
      .map(() => ctx.add(cvar(d.name, ty))));
  } else if(d instanceof DData) {
    console.log(''+d);
    const name = d.name;
    const params = d.params;
    const constrs = d.constrs;
    return noDups(params.map(([n, _]) => n))
      .then(() => noDups(constrs.map(([n, _]) => n)))
      .then(() => {
        for(let i = 0; i < params.length; i++) {
          const r = kindWF(ctx, params[i][1]);
          if(isErr(r)) return new Err(r.err);
        }
        for(let i = 0; i < constrs.length; i++) {
          const c = constrs[i];
          const n = c[0];
          const ts = c[1];
          for(let j = 0; j < ts.length; j++) {
            if(ts[j].occursNegatively(n, false))
              return err(`${n} occurs in a negative position in ${ts[j]}`);
          }
        }
        const r = fresh(params.map(([n, _]) => n), 'r');
        return ok(ctx.add(ctcon(name, d.getKind())).append(new Context(
          constrs.map(([n, ts]) => cvar(n, tforalls(params, tfuns.apply(null, ts.concat([d.getType()]))))))
          ).add(
            cvar(`case${d.name}`, tforalls(params, tforalls([[r, ktype]],
              tfuns.apply(null, constrs.map(([n, ts]) => tfuns.apply(null, ts.concat([tvar(r)]))).concat([d.getType(), tvar(r)]))))),
            cvar(`fold${d.name}`, tforalls(params, tforalls([[r, ktype]],
              tfuns.apply(null, constrs.map(([n, ts]) => tfuns.apply(null, ts.map(t => t.equals(d.getType())? tvar(r): t).concat([tvar(r)]))).concat([d.getType(), tvar(r)])))))
          ));
      })
      .then((ctx: Context) => contextWF(ctx).map(() => ctx));
  }
  return impossible();
}

export function inferProgram(ctx: Context, ds: Definition[]): IResult<Context> {
  let c = ctx;
  for(let i = 0; i < ds.length; i++) {
    const d = ds[i];
    const r = inferDefinition(c, d);
    if(isErr(r)) return new Err(r.err);
    else if(isOk(r)) {
      console.log(''+d, ''+r.val);
      c = r.val;
    } else impossible();
  }
  return ok(c);
}
