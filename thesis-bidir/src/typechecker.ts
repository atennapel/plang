import {
  Type,
  TVar,
  TUnit,
  TExists,
  texists,
  TArr,
  TForall,
} from './types';
import {
  Result
} from './Result';
import Context, {
  cexists,
  cmarker,
  cforall,
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
}

function instL(st: InferState, c: Context, a: Id, b: Type): InferResult<{st: InferState, c: Context}> {
  return err('instL unimplemented');
}
function instR(st: InferState, c: Context, a: Type, b: Id): InferResult<{st: InferState, c: Context}> {
  return err('instR unimplemented');
}

export function subtype(st: InferState, c: Context, a: Type, b: Type): InferResult<{st: InferState, c: Context}> {
  if(!c.isWellformed()) return err(`ill-formed context: ${c}`);
  if(!a.isWellformed(c)) return err(`ill-formed type: ${a} in ${c}`);
  if(!b.isWellformed(c)) return err(`ill-formed type: ${b} in ${c}`);
  if(a instanceof TVar && b instanceof TVar) return ok({st, c});
  if(a instanceof TUnit && b instanceof TUnit) return ok({st, c});
  if(a instanceof TExists && b instanceof TExists) {
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
    return subtype(st, c.append(b.tvar), a, b.type)
      .then(({st, c}) => c.split(cforall(b.tvar.id))
      .then(({left: c}) => ok({st, c})));
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
