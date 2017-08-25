import {
  Type,
  TVar,
  tvar,
  TUnit,
  tunit,
  TExists,
  texists,
  TArr,
  tarr,
  TForall,
  tforall,
} from './types';
import {
  Expr,
  EAnno,
  EVar,
  ELam,
  EApp,
  EUnit,
} from './exprs';
import {
  Result,
  Ok,
  Err,
} from './Result';
import Context, {
  cexists,
  cmarker,
  cforall,
  csolved,
  cvar,
  CVar,
} from './Context';
import Id, { IdStore } from './Id';

type InferResult<T> = Result<TypeError, T>;
function ok<T>(val: T): InferResult<T> {
  return Result.ok(val);
}
function err<T>(msg: string): InferResult<T> {
  return Result.err(new TypeError(msg));
}

export class InferState {
  private readonly tvars: IdStore;

  constructor(tvars: IdStore) {
    this.tvars = tvars;
  }

  static empty() {
    return new InferState(new IdStore());
  }

  fresh(name: string): InferResult<{st: InferState, id: Id}> {
    const {st, id} = this.tvars.fresh(name);
    return ok({st: new InferState(st), id});
  }

  freshN(n: number, names: string | string[]): InferResult<{st: InferState, ids: Id[]}> {
    const {st, ids} = this.tvars.freshN(n, names);
    return ok({st: new InferState(st), ids});
  }
}

function instL(st: InferState, c: Context, a: Id, b: Type): InferResult<{st: InferState, c: Context}> {
  console.log(`instL: ${a} and ${b} in ${c}`);
  if(!c.isWellformed()) return err(`ill-formed context: ${c}`);
  if(!b.isWellformed(c)) return err(`ill-formed type: ${b} in ${c}`);
  if(!texists(a).isWellformed(c)) return err(`ill-formed type: ${texists(a)} in ${c}`);
  if(b.isMono()) {
    const r = c.solve(a, b).map(c => ({st, c}));
    if(r instanceof Ok) return r;
  }
  if(b instanceof TExists) {
    if(!c.contains(cexists(a)))
      return err(`context does not contain existential: ${a} in ${c}`);
    if(!c.contains(cexists(b.id)))
      return err(`context does not contain existential: ${b} in ${c}`);
    if(c.indexOf(cexists(a)) > c.indexOf(cexists(b.id)))
      return err(`unordered: ${a} and ${b} in ${c}`);
    return c.solve(b.id, texists(a)).map(c => ({st, c}));
  } else if(b instanceof TForall) {
    if(!c.contains(cexists(a)))
      return err(`context does not contain existential: ${a} in ${c}`);
    return st.fresh('t')
      .then(({st, id: alpha}) =>
        instL(st, c.append(cforall(alpha)), a, b.type.subst(b.tvar.id, tvar(alpha)))
      .then(({st, c}) => c.split(cforall(alpha))
      .map(({left: c}) => ({st, c}))));
  } else if(b instanceof TArr) {
    if(!c.contains(cexists(a)))
      return err(`context does not contain existential: ${a} in ${c}`);
    return st.fresh('a')
      .then(({st, id: a1}) => st.fresh('a')
      .then(({st, id: a2}) => c.insertAt(cexists(a), [
        cexists(a2),
        cexists(a1),
        csolved(a, tarr(texists(a1), texists(a2)))
      ])
      .then(c => instR(st, c, b.left, a1)
      .then(({st, c}) => instL(st, c, a2, b.right.apply(c))))));
  } else {
    return err(`something went wrong in instL: ${a} and ${b} in ${c}`);
  }
}
function instR(st: InferState, c: Context, a: Type, b: Id): InferResult<{st: InferState, c: Context}> {
  console.log(`instR: ${a} and ${b} in ${c}`);
  if(!c.isWellformed()) return err(`ill-formed context: ${c}`);
  if(!a.isWellformed(c)) return err(`ill-formed type: ${a} in ${c}`);
  if(!texists(b).isWellformed(c)) return err(`ill-formed type: ${texists(b)} in ${c}`);
  if(a.isMono()) {
    const r = c.solve(b, a).map(c => ({st, c}));
    if(r instanceof Ok) return r;
  }
  if(a instanceof TExists) {
    if(!c.contains(cexists(b)))
      return err(`context does not contain existential: ${b} in ${c}`);
    if(!c.contains(cexists(a.id)))
      return err(`context does not contain existential: ${a} in ${c}`);
    if(c.indexOf(cexists(b)) > c.indexOf(cexists(a.id)))
      return err(`unordered: ${a} and ${b} in ${c}`);
    return c.solve(a.id, texists(b)).map(c => ({st, c}));
  } else if(a instanceof TForall) {
    if(!c.contains(cexists(b)))
      return err(`context does not contain existential: ${b} in ${c}`);
    return st.fresh('b')
      .then(({st, id: beta}) =>
        instR(st, c.append(cmarker(beta), cexists(beta)), a.type.subst(a.tvar.id, texists(beta)), b)
      .then(({st, c}) => c.split(cmarker(beta))
      .map(({left: c}) => ({st, c}))));
  } else if(a instanceof TArr) {
    if(!c.contains(cexists(b)))
      return err(`context does not contain existential: ${b} in ${c}`);
    return st.fresh('a')
      .then(({st, id: a1}) => st.fresh('a')
      .then(({st, id: a2}) => c.insertAt(cexists(b), [
        cexists(a2),
        cexists(a1),
        csolved(b, tarr(texists(a1), texists(a2)))
      ])
      .then(c => instL(st, c, a1, a.left)
      .then(({st, c}) => instR(st, c, a.right.apply(c), a2)))));
  } else {
    return err(`something went wrong in instR: ${a} and ${b} in ${c}`);
  }
}

function subtype(st: InferState, c: Context, a: Type, b: Type): InferResult<{st: InferState, c: Context}> {
  console.log(`subtype: ${a} and ${b} in ${c}`);
  if(!c.isWellformed()) return err(`ill-formed context: ${c}`);
  if(!a.isWellformed(c)) return err(`ill-formed type: ${a} in ${c}`);
  if(!b.isWellformed(c)) return err(`ill-formed type: ${b} in ${c}`);
  if(a instanceof TVar && b instanceof TVar && a.id.equals(b.id)) return ok({st, c});
  if(a instanceof TUnit && b instanceof TUnit) return ok({st, c});
  if(a instanceof TExists && b instanceof TExists && a.id.equals(b.id)) {
    if(!c.contains(cexists(a.id)))
      return err(`context does not contain existential: ${a} in ${c}`);
    return ok({st, c});
  }
  if(a instanceof TArr && b instanceof TArr) {
    return subtype(st, c, b.left, a.left)
      .then(({st, c}) => subtype(st, c, a.right.apply(c), b.right.apply(c)));
  }
  if(a instanceof TForall) {
    return st.fresh('t')
      .then(({st, id: alpha}) =>
        subtype(st, c.append(cmarker(alpha), cexists(alpha)), a.type.subst(a.tvar.id, texists(alpha)), b)
      .then(({st, c}) => c.split(cmarker(alpha))
      .then(({left: c}) => ok({st, c}))));
  }
  if(b instanceof TForall) {
    return st.fresh('t')
      .then(({st, id: alpha}) =>
        subtype(st, c.append(cforall(alpha)), a, b.type.subst(b.tvar.id, tvar(alpha)))
      .then(({st, c}) => c.split(cforall(alpha))
      .then(({left: c}) => ok({st, c}))));
  }
  if(a instanceof TExists) {
    if(!c.contains(cexists(a.id)))
      return err(`context does not contain existential: ${a} in ${c}`);
    if(b.contains(a.id))
      return err(`recursive type: ${a} and ${b}`);
    return instL(st, c, a.id, b);
  }
  if(b instanceof TExists) {
    if(!c.contains(cexists(b.id)))
      return err(`context does not contain existential: ${b} in ${c}`);
    if(a.contains(b.id))
      return err(`recursive type: ${a} and ${b}`);
    return instR(st, c, a, b.id);
  }
  return err(`subtype failed: ${a} <: ${b}`);
}

function typecheck(st: InferState, c: Context, e: Expr, t: Type): InferResult<{st: InferState, c: Context}> {
  console.log(`typecheck: ${e} and ${t} in ${c}`);
  if(!c.isWellformed()) return err(`ill-formed context: ${c}`);
  if(!t.isWellformed(c)) return err(`ill-formed type: ${t} in ${c}`);
  if(e instanceof EUnit && t instanceof TUnit) return ok({st, c});
  if(t instanceof TForall) {
    return st.fresh('t')
      .then(({st, id: alpha}) =>
        typecheck(st, c.append(cforall(alpha)), e, t.type.subst(t.tvar.id, tvar(alpha)))
      .then(({st, c}) => c.split(cforall(alpha))
      .map(({left: c}) => ({st, c}))));
  }
  if(e instanceof ELam && t instanceof TArr) {
    return typecheck(st, c.append(cvar(e.arg, t.left)), e.body, t.right)
      .then(({st, c}) => c.split(x => x instanceof CVar && x.id === e.arg)
      .map(({left: c}) => ({st, c})));
  }
  return typesynth(st, c, e)
    .then(({st, c, t: t2}) => subtype(st, c, t2.apply(c), t.apply(c)));
}

function typesynth(st: InferState, c: Context, e: Expr): InferResult<{st: InferState, c: Context, t: Type}> {
  console.log(`typesynth: ${e} in ${c}`);
  if(!c.isWellformed()) return err(`ill-formed context: ${c}`);
  if(e instanceof EUnit) return ok({st, c, t: tunit});
  if(e instanceof EAnno) {
    return typecheck(st, c, e.expr, e.type)
      .map(({st, c}) => ({st, c, t: e.type}));
  }
  if(e instanceof EVar) {
    return c.get(e.id).map(t => ({st, c, t}));
  }
  if(e instanceof EApp) {
    return typesynth(st, c, e.left)
      .then(({st, c, t}) => typeapplysynth(st, c, t.apply(c), e.right));
  }
  if(e instanceof ELam) {
    return st.fresh('a')
      .then(({st, id: alpha}) => st.fresh('b')
      .then(({st, id: beta}) =>
        typecheck(st, c.append(
          cmarker(alpha),
          cexists(alpha),
          cexists(beta),
          cvar(e.arg, texists(alpha))
        ), e.body, texists(beta))
      .then(({st, c}) => c.split(cmarker(alpha))
      .then(({left, right}) => {
        const tau = tarr(texists(alpha), texists(beta)).apply(right);
        const vars = right.unsolved();
        return st.freshN(vars.length, vars.map(v => v.name))
          .map(({st, ids}) => {
            const sub = ids.map((id, i) => [vars[i], tvar(id)] as [Id, Type]);
            return {st, c: left, t: tforall(ids.map(tvar), tau.substAll(sub))};
          });
      }))));
  }
  return err(`typesynth unhandled expression: ${e}`);
}

function typeapplysynth(st: InferState, c: Context, t: Type, e: Expr): InferResult<{st: InferState, c: Context, t: Type}> {
  console.log(`typeapplysynth: ${t} and ${e} in ${c}`);
  if(!c.isWellformed()) return err(`ill-formed context: ${c}`);
  if(!t.isWellformed(c)) return err(`ill-formed type: ${t} in ${c}`);
  if(t instanceof TForall) {
    return st.fresh('a')
      .then(({st, id: alpha}) =>
        typeapplysynth(st, c.append(cexists(alpha)), t.type.subst(t.tvar.id, texists(alpha)), e));
  }
  if(t instanceof TExists) {
    return st.fresh('a')
      .then(({st, id: a1}) => st.fresh('a')
      .then(({st, id: a2}) => c.insertAt(cexists(t.id), [
        cexists(a2),
        cexists(a1),
        csolved(t.id, tarr(texists(a1), texists(a2)))
      ])
      .then(c => typecheck(st, c, e, texists(a1))
      .map(({st, c}) => ({st, c, t: texists(a2)})))));
  }
  if(t instanceof TArr) {
    return typecheck(st, c, e, t.left)
      .map(({st, c}) => ({st, c, t: t.right}));
  }
  return err(`invalid typeapplysynth: ${t} and ${e} in ${c}`);
}

function generalize(st: InferState, t: Type): InferResult<{st: InferState, t: Type}> {
  const vars = t.free().values();
  return st.freshN(vars.length, vars.map(v => v.name))
    .map(({st, ids}) => {
      const sub = ids.map((id, i) => [vars[i], tvar(id)] as [Id, Type]);
      return {st, t: tforall(ids.map(tvar), t.substAll(sub))};
    });
}

export function infer(e: Expr): InferResult<{t: Type, c: Context}> {
  return typesynth(InferState.empty(), Context.empty(), e)
    .then(({st, c, t}) => generalize(st, t.apply(c))
    .map(({st, t}) => ({c, t})));
}
