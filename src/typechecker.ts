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
  EQuery,
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
const err = <T>(msg: string): T => { throw new TypeError(msg) };
const check = (b: boolean, m: string): null => b? null: err(m);

function noDups(d: string[]): null {
  const o: { [key: string]: boolean } = {};
  for(let i = 0; i < d.length; i++) {
    if(o[d[i]]) return err(`duplicate ${d[i]}`);
    o[d[i]] = true;
  }
  return null;
}

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
const findVar = (ctx: Context, name: string): Type => {
  const r = ctx.findVar(name);
  return r === null? err(`var ${name} not found in ${ctx}`): (r);
};
const findSolved = (ctx: Context, name: string): Type => {
  const r = ctx.findSolved(name);
  return r === null? err(`var ${name} not found in ${ctx}`): (r);
};
const findKCon = (ctx: Context, name: string): null => {
  return ctx.findKCon(name) === null? err(`tcon ${name} not found in ${ctx}`): (null);
};
const findTCon = (ctx: Context, name: string): Kind => {
  const r = ctx.findTCon(name);
  return r === null? err(`tcon ${name} not found in ${ctx}`): (r);
};
const findTVar = (ctx: Context, name: string): Kind => {
  const r = ctx.findTVar(name);
  return r === null? err(`tvar ${name} not found in ${ctx}`): (r);
};
const findEx = (ctx: Context, name: string): Kind => {
  const r = ctx.findEx(name);
  return r === null? err(`ex ^${name} not found in ${ctx}`): (r);
};
const findMarker = (ctx: Context, name: string): null => {
  return ctx.findMarker(name) === null? err(`marker |>${name} not found in ${ctx}`): (null);
};
const findExOrSolved = (ctx: Context, name: string): Kind => {
  const r = ctx.findExOrSolved(name);
  return r === null? err(`ex or solved ^${name} not found in ${ctx}`): (r);
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
function checkKindType(kind: Kind): null {
  return ktype.equals(kind)? (null): err(`kind is not ${ktype}: ${kind}`);
}

function kindWF(ctx: Context, kind: Kind): null {
  // console.log(`kindWF ${kind} in ${ctx}`);
  if(kind instanceof KCon) return findKCon(ctx, kind.name);
  if(kind instanceof KFun) {
    kindWF(ctx, kind.left);
    return kindWF(ctx, kind.right); 
  }
  return impossible();
}

function typeWF(ctx: Context, ty: Type): Kind {
  //console.log(`typeWF ${ty} in ${ctx}`);
  if(ty instanceof TCon) {
    const k = findTCon(ctx, ty.name);
    kindWF(ctx, k);
    return k;
  }
  if(ty instanceof TVar) {
    const k = findTVar(ctx, ty.name);
    kindWF(ctx, k);
    return k;
  }
  if(ty instanceof TEx) {
    const k = findExOrSolved(ctx, ty.name);
    kindWF(ctx, k);
    return k;
  }
  if(ty instanceof TFun) {
    const k1 = typeWF(ctx, ty.left);
    checkKindType(k1);
    const k2 = typeWF(ctx, ty.right);
    checkKindType(k2);
    return ktype;
  }
  if(ty instanceof TForall) {
    kindWF(ctx, ty.kind);
    return typeWF(ctx.add(ctvar(ty.name, ty.kind)), ty.type);
  }
  if(ty instanceof TApp) {
    const kleft = typeWF(ctx, ty.left);
    if(kleft instanceof KFun) {
      const kright = typeWF(ctx, ty.right);
      if(!kright.equals(kleft.left))
        return err(`kind mismatch in type constructor: ${ty} in ${ctx}`);
      return (kleft.right);
    }
    else return err(`not a type constructor: ${ty} in ${ctx}`);
  }
  return impossible();
}

function contextWF(ctx: Context): null {
  // console.log(`contextWF ${ctx}`);
  const a = ctx.elems;
  const l = a.length;
  for(let i = 0; i < l; i++) {
    const e = a[i];
    const p = new Context(a.slice(0, i));
    if(e instanceof CKCon) {
      if(p.findKCon(e.name) !== null) return err(`duplicate kcon ^${e.name}`);
    } else if(e instanceof CTCon) {
      if(p.findTCon(e.name) !== null) return err(`duplicate tcon ${e.name}`);
      kindWF(p, e.kind);
    } else if(e instanceof CTVar) {
      if(p.findTVar(e.name) !== null) return err(`duplicate tvar ${e.name}`);
      kindWF(p, e.kind);
    } else if(e instanceof CTEx || e instanceof CSolved) {
      if(p.findExOrSolved(e.name) !== null) return err(`duplicate tex ^${e.name}`);
      kindWF(p, e.kind);
    } else if(e instanceof CVar) {
      if(p.findVar(e.name) !== null) return err(`duplicate var ${e.name}`);
      const k = typeWF(p, e.type);
      checkKindType(k);
    } else if(e instanceof CMarker) {
      if(p.findMarker(e.name) !== null) return err(`duplicate marker ^${e.name}`);
      if(p.findExOrSolved(e.name) !== null) return err(`duplicate marker ^${e.name}`);
    } else return impossible();
  }
  return (null);
}

// subtype
function subtype(ctx: Context, a: Type, b: Type): Context {
  // console.log(`subtype ${a} and ${b} in ${ctx}`);
  const k = typeWF(ctx, a)
  const k2 = typeWF(ctx, b);
  if(!k.equals(k2))
    return err(`kind mismatch ${a} and ${b}, ${k} and ${k2} in ${ctx}`);
  if(((a instanceof TVar && b instanceof TVar) ||
    (a instanceof TEx && b instanceof TEx) ||
    (a instanceof TCon && b instanceof TCon)) && a.name === b.name)
    return (ctx);
  if(a instanceof TApp && b instanceof TApp) {
    const ctx_ = subtype(ctx, a.left, b.left);
    return subtype(ctx_, ctx_.apply(a.right), ctx_.apply(b.right));
  }
  if(a instanceof TFun && b instanceof TFun) {
    const ctx_ = subtype(ctx, b.left, a.left);
    return subtype(ctx_, ctx_.apply(a.right), ctx_.apply(b.right));
  }
  if(a instanceof TForall) {
    const x = fresh(ctx.texs(), a.name);
    const ctx_ = subtype(ctx.add(cmarker(x), ctex(x, a.kind)), a.open(tex(x)), b);
    return (ctx_.split(isCMarker(x)).left);
  }
  if(b instanceof TForall) {
    const x = fresh(ctx.tvars(), b.name);
    const ctx_ = subtype(ctx.add(ctvar(x, b.kind)), a, b.open(tvar(x)));
    return (ctx_.split(isCTVar(x)).left);
  }
  if(a instanceof TEx) {
    const r1 = findEx(ctx, a.name);
    const r2 = check(!b.containsEx(a.name), `occurs check failed L: ${a} in ${b}`);
    return instL(ctx, a.name, b); 
  }
  if(b instanceof TEx) {
    const r1 = findEx(ctx, b.name);
    const r2 = check(!a.containsEx(b.name), `occurs check failed R: ${b} in ${a}`);
    return instR(ctx, a, b.name);
  }
  return err(`subtype failed: ${a} <: ${b} in ${ctx}`);
}

// inst
function solve(ctx: Context, name: string, ty: Type): Context {
  // console.log(`solve ${name} and ${ty} in ${ctx}`);
  if(ty.isMono()) {
    const s = ctx.split(isCTEx(name));
    const k = typeWF(s.left, ty);
    return (s.left.add(csolved(name, k, ty)).append(s.right));
  } else return err(`polymorphic type in solve: ${name} := ${ty} in ${ctx}`);
}

function instL(ctx: Context, a: string, b: Type): Context {
  // console.log(`instL ${a} and ${b} in ${ctx}`);
  if(b instanceof TEx && ctx.isOrdered(a, b.name)) return solve(ctx, b.name, tex(a));
  if(b instanceof TEx && ctx.isOrdered(b.name, a)) return solve(ctx, a, b);
  try {
    return solve(ctx, a, b);
  } catch(e) {
    if(!(e instanceof TypeError)) throw e;
  }
  if(b instanceof TApp) {
    const kf = typeWF(ctx, b.left) as KFun;
    const texs = ctx.texs();
    const a1 = fresh(texs, a);
    const a2 = fresh(texs.concat([a1]), a);
    const ctx_ = instL(ctx.replace(isCTEx(a), new Context([ctex(a2, kf.left), ctex(a1, kf), csolved(a, kf.right, tapp(tex(a1), tex(a2)))])), a1, b.left);
    return instL(ctx_, a2, ctx_.apply(b.right));
  }
  if(b instanceof TFun) {
    const texs = ctx.texs();
    const a1 = fresh(texs, a);
    const a2 = fresh(texs.concat([a1]), a);
    const ctx_ = instR(ctx.replace(isCTEx(a), new Context([ctex(a2, ktype), ctex(a1, ktype), csolved(a, ktype, tfun(tex(a1), tex(a2)))])), b.left, a1);
    return instL(ctx_, a2, ctx_.apply(b.right));
  }
  if(b instanceof TForall) {
    const x = fresh(ctx.tvars(), b.name);
    const ctx_ = instL(ctx.add(ctvar(x, b.kind)), a, b.open(tvar(x)));
    return (ctx_.split(isCTVar(x)).left);
  }
  return err(`instL failed: ${a} and ${b} in ${ctx}`);
}

function instR(ctx: Context, a: Type, b: string): Context {
  // console.log(`instR ${a} and ${b} in ${ctx}`);
  if(a instanceof TEx && ctx.isOrdered(b, a.name)) return solve(ctx, a.name, tex(b));
  if(a instanceof TEx && ctx.isOrdered(a.name, b)) return solve(ctx, b, a);
  try {
    return solve(ctx, b, a);
  } catch(e) {
    if(!(e instanceof TypeError)) throw e;
  }
  if(a instanceof TApp) {
    const kf = typeWF(ctx, a.left) as KFun;
    const texs = ctx.texs();
    const b1 = fresh(texs, b);
    const b2 = fresh(texs.concat([b1]), b);
    const ctx_ = instR(ctx.replace(isCTEx(b), new Context([ctex(b2, kf.left), ctex(b1, kf), csolved(b, kf.right, tapp(tex(b1), tex(b2)))])), a.left, b1);
    return instR(ctx_, ctx_.apply(a.right), b2);
  }
  if(a instanceof TFun) {
    const texs = ctx.texs();
    const b1 = fresh(texs, b);
    const b2 = fresh(texs.concat([b1]), b);
    const ctx_ = instL(ctx.replace(isCTEx(b), new Context([ctex(b2, ktype), ctex(b1, ktype), csolved(b, ktype, tfun(tex(b1), tex(b2)))])), b1, a.left);
    return instR(ctx_, ctx_.apply(a.right), b2);
  }
  if(a instanceof TForall) {
    const x = fresh(ctx.texs(), a.name);
    const ctx_ = instR(ctx.add(cmarker(x), ctex(x, a.kind)), a.open(tex(x)), b);
    return (ctx_.split(isCMarker(x)).left);
  }
  return err(`instR failed: ${a} and ${b} in ${ctx}`);
}

// synth/check
function synth(ctx: Context, e: Expr): { ctx: Context, ty: Type } {
  // console.log(`synth ${e} in ${ctx}`);
  const r = contextWF(ctx);
  if(e instanceof EVar) {
    const ty = findVar(ctx, e.name);
    return { ctx, ty };
  }
  if(e instanceof EAbs) {
    if(e.isAnnotated()) {
      const ty = e.type as Type;
      const k = typeWF(ctx, ty);
      checkKindType(k);
      const x = fresh(ctx.vars(), e.name);
      const b = fresh(ctx.texs(), e.name);
      const ctx_ = checkTy(ctx.add(cmarker(b), ctex(b, ktype), cvar(x, ty)), e.open(evar(x)), tex(b));
      const s = ctx_.split(isCMarker(b));
      const t = s.right.apply(tfun(ty, tex(b)));
      const u = orderedTExs(s.right.unsolved(), t);
      return ({
        ctx: s.left,
        ty: tforalls(u, u.reduce((t, [n, _]) => t.substEx(n, tvar(n)), t)),
      });
    } else {
      const x = fresh(ctx.vars(), e.name);
      const texs = ctx.texs();
      const a = fresh(texs, e.name);
      const b = fresh(texs.concat([a]), e.name);
      const ctx_ = checkTy(ctx.add(cmarker(a), ctex(a, ktype), ctex(b, ktype), cvar(x, tex(a))), e.open(evar(x)), tex(b));
      const s = ctx_.split(isCMarker(a));
      console.log(''+s.right);
      const t = s.right.apply(tfun(tex(a), tex(b)));
      const u = orderedTExs(s.right.unsolved(), t);
      return ({
        ctx: s.left,
        ty: tforalls(u, u.reduce((t, [n, _]) => t.substEx(n, tvar(n)), t)),
      });
    }
  }
  if(e instanceof EApp) {
    const r = synth(ctx, e.left);
    return synthapp(r.ctx, r.ctx.apply(r.ty), e.right);
  }
  if(e instanceof EAnno) {
    typeWF(ctx, e.type);
    const ctx_ = checkTy(ctx, e.expr, e.type);;
    return { ctx: ctx_, ty: e.type };
  }
  if(e instanceof ETAbs) {
    kindWF(ctx, e.kind);
    const x = fresh(ctx.vars(), e.name);
    const r = synth(ctx.add(ctvar(x, e.kind)), e.expr.substType(e.name, tvar(x)));
    return ({ ctx: r.ctx, ty: tforall(x, e.kind, r.ctx.apply(r.ty)) });
  }
  if(e instanceof ETApp) {
    typeWF(ctx, e.type);
    const r = synth(ctx, e.expr);
    const ty_ = r.ctx.apply(r.ty);
    return ty_ instanceof TForall? ({ ctx: r.ctx, ty: ty_.open(e.type) }):
      err(`type application on non-polymorphic type: ${e} with ${ty_} in ${r.ctx}`);
  }
  if(e instanceof EQuery) {
    const q = fresh(ctx.texs(), 'q');
    return ({ ctx: ctx.add(ctex(q, ktype)), ty: tex(q) });
  }
  return err(`cannot synth ${e} in ${ctx}`);
}

function checkTy(ctx: Context, e: Expr, ty: Type): Context {
  // console.log(`checkTy ${e} and ${ty} in ${ctx}`);
  const r = contextWF(ctx);
  if(e instanceof EQuery) {
    const q = fresh(ctx.texs(), 'q');
    return (ctx.add(csolved(q, ktype, ty)));
  }
  if(ty instanceof TForall) {
    const x = fresh(ctx.tvars(), ty.name);
    const ctx_ = checkTy(ctx.add(ctvar(x, ty.kind)), e, ty.open(tvar(x)));
    return (ctx_.split(isCTVar(x)).left);
  }
  if(e instanceof EAbs && !e.isAnnotated() && ty instanceof TFun) {
    const x = fresh(ctx.vars(), e.name);
    const ctx_ = checkTy(ctx.add(cvar(x, ty.left)), e.open(evar(x)), ty.right);
    return (ctx_.split(isCVar(x)).left);
  }
  /*if(e instanceof EQuery) {
    const evars = ctx.elems.filter(e => e instanceof CVar) as CVar[];
    const res = evars.map(e => ({ name: e.name, res: subtype(ctx, e.type, ty) }));
    const found: { name: string, res: IResult<Context> }[] = [];
    for(let i = res.length - 1; i >= 0; i--) {
      const c = res[i];
      if(is(c.res)) found.push(c);
    }
    if(found.length === 0)
      return err(`no implicit value found for ${ty} in ${ctx}`);
    if(found.length === 1) {
      (e as any)._impl = found[0].name;
      console.log(`implicit found ${found[0].name} for ${ty}`);
      return (ctx);
    }
    return err(`multiple implicit values found for ${ty}: [${found.map(x => x.name).join(', ')}] in ${ctx}`);
  }*/
  const rr = synth(ctx, e);
  return subtype(rr.ctx, rr.ctx.apply(rr.ty), rr.ctx.apply(ty));
}

function synthapp(ctx: Context, ty: Type, e: Expr): { ctx: Context, ty: Type } {
  // console.log(`synthapp ${ty} and ${e} in ${ctx}`);
  const r = contextWF(ctx);
  if(ty instanceof TForall) {
    const x = fresh(ctx.texs(), ty.name);
    return synthapp(ctx.add(ctex(x, ty.kind)), ty.open(tex(x)), e);
  }
  if(ty instanceof TEx) {
    findEx(ctx, ty.name);
    const texs = ctx.texs();
    const a1 = fresh(texs, ty.name);
    const a2 = fresh(texs.concat([a1]), ty.name);
    const ctx_ = checkTy(ctx.replace(
      isCTEx(ty.name),
      new Context([ctex(a2, ktype), ctex(a1, ktype), csolved(ty.name, ktype, tfun(tex(a1), tex(a2)))])),
      e, tex(a1)
    );
    return ({ ctx: ctx_, ty: tex(a2) });
  }
  if(ty instanceof TFun) {
    const ctx_ = checkTy(ctx, e, ty.left);
    return ({ ctx: ctx_, ty: ty.right });
  }
  return err(`cannot synthapp ${ty} with ${e} in ${ctx}`);
}

export function infer(ctx: Context, e: Expr): { ctx: Context, ty: Type } {
  const r = synth(ctx, e);
  const ctx__ = r.ctx;
  const ty = r.ty;
  contextWF(ctx__);
  const ctx_ = ctx__.applyContext(ctx__);
  const ty_ = ctx_.apply(ty);
  const k = typeWF(ctx_, ty_);
  checkKindType(k);
  console.log(''+new Context(ctx_.elems.filter(e => (e instanceof CTEx || e instanceof CSolved) && e.name.startsWith('q'))));
  if(ctx_.isComplete()) return ({ ctx: ctx_, ty: ty_ });
  const unsolved = ctx_.unsolved();
  const unsolvedNames = unsolved.map(([n, _]) => n);
  const u = orderedTExs(unsolved, ty_);
  return ({
    ctx: ctx_.removeAll(e => (e instanceof CSolved) || ((e instanceof CTEx || e instanceof CMarker) && unsolvedNames.indexOf(e.name) >= 0)),
    ty: tforalls(u, u.reduce((t, [n, _]) => t.substEx(n, tvar(n)), ty_)),
  });
}

export function inferDefinition(ctx: Context, d: Definition): Context {
  if(d instanceof DValue) {
    console.log(''+d);
    const r = infer(ctx, (d.type? eanno(d.val, d.type): d.val));
    const ty_ = r.ty;
    const ctx_ = r.ctx.removeAll(e => e instanceof CSolved).add(cvar(d.name, ty_));
    contextWF(ctx_);
    return ctx_;
  } else if(d instanceof DData) {
    console.log(''+d);
    const name = d.name;
    const params = d.params;
    const constrs = d.constrs;
    noDups(params.map(([n, _]) => n));
    noDups(constrs.map(([n, _]) => n));
    for(let i = 0; i < params.length; i++) {
      kindWF(ctx, params[i][1]);
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
    const ctx_ = (ctx.add(ctcon(name, d.getKind())).append(new Context(
      constrs.map(([n, ts]) => cvar(n, tforalls(params, tfuns.apply(null, ts.concat([d.getType()]))))))
      ).add(
        cvar(`case${d.name}`, tforalls(params, tforalls([[r, ktype]],
          tfuns.apply(null, constrs.map(([n, ts]) => tfuns.apply(null, ts.concat([tvar(r)]))).concat([d.getType(), tvar(r)]))))),
        cvar(`cata${d.name}`, tforalls(params, tforalls([[r, ktype]],
          tfuns.apply(null, constrs.map(([n, ts]) => tfuns.apply(null, ts.map(t => t.equals(d.getType())? tvar(r): t).concat([tvar(r)]))).concat([d.getType(), tvar(r)]))))),
        cvar(`para${d.name}`, tforalls(params, tforalls([[r, ktype]],
          tfuns.apply(null, constrs.map(([n, ts]) => tfuns.apply(null, ts.map(t => t.equals(d.getType())? [t, tvar(r)]: [t]).reduce((a, b) => a.concat(b), []).concat([tvar(r)]))).concat([d.getType(), tvar(r)])))))
      ));
    contextWF(ctx_);
    return ctx_;
  }
  return impossible();
}

export function inferProgram(ctx: Context, ds: Definition[]): Context {
  let c = ctx;
  for(let i = 0; i < ds.length; i++) {
    const d = ds[i];
    const r = inferDefinition(c, d);
    c = r;
  }
  return (c);
}
