import Map from './Map';
import Subst from './Subst';
import { Type, TVar } from './types';

export default class TVarSet {
	private readonly _map: Map<TVar>;
	
	constructor(_map: Map<TVar>) {
		this._map = _map;
	}

	static from(tvars: TVar[]) {
		return new TVarSet(Map.from(tvars.map(tv => [tv.hash(), tv] as [string, TVar])));
	}
	static of(...tvars: TVar[]) {
		return TVarSet.from(tvars);
	}
	static empty<T>() {
		return new TVarSet(Map.empty());
	}

	ids() {
		return this._map.keys();
	}

	values() {
		return this._map.values();
	}
	size() {
		return this._map.size();
	}

	toString() {
		return `{${this._map.values().map(x => x.toString()).join(', ')}}`;
	}

	has(tv: TVar): boolean {
		return this._map.has(tv.hash());
	}

	union(other: TVarSet): TVarSet {
		return new TVarSet(this._map.union(other._map));
	}
	without(other: TVarSet): TVarSet {
		return new TVarSet(this._map.removeKeys(other._map.keys()));
	}

	toSubst(fn: (tv: TVar) => Type): Subst {
		return new Subst(this._map.map(tv => fn(tv)));
	}
}
