import { Type, TScheme, TVar } from './types';
import { EVar } from './exprs';
import { Result } from './Result';
import { IdStore } from './Id';
import { Kind, KType } from './kinds';
import Map from './Map';

export type Subst = Map<TVar, Type>;
export const emptySubst: Subst = Map.empty<TVar, Type>();
export function compose(s1: Subst, s2: Subst): Subst {
  return s2.union(s1.map(v => v.subst(s2)));
}

export type Env = Map<EVar, TScheme>;
export const emptyEnv: Env = Map.empty<EVar, TScheme>();

export type InferResult<T> = Result<TypeError, T>;
export function ok<T>(v: T): InferResult<T> { return Result.ok(v) };
export function err<T>(m: string): InferResult<T> { return Result.err(new TypeError(m)) };

export class InferState {
	private readonly tvarstore: IdStore;
	
	constructor(tvarstore: IdStore) {
		this.tvarstore = tvarstore;
	}

	public static empty() {
		return new InferState(new IdStore());
	}

	public freshTVar(oldTVar?: TVar | string, kind?: Kind, lacks?: string[]): [InferState, TVar] {
		const [store, id] = oldTVar instanceof TVar? this.tvarstore.freshId(oldTVar.id): this.tvarstore.fresh(oldTVar);
		return [new InferState(store), new TVar(id, kind || KType, lacks || [])];
	}
}
