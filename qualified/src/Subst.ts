import { Type, TVar } from './types';
import Map from './Map';
import HasTVars from './HasTVars';
import TVarSet from './TVarSet';

export default class Subst implements HasTVars<Subst> {
	private readonly _map: Map<Type>;

	constructor(_map: Map<Type>) {
		this._map = _map;
	}

	static from(subs: [TVar, Type][]): Subst {
		return new Subst(Map.from(subs.map(([v, t]) => [v.hash(), t] as [string, Type])));
	}
	static of(...subs: [TVar, Type][]): Subst {
		return this.from(subs);
	}
	static empty() {
		return new Subst(Map.empty());
	}

	toString() {
		return this._map.toString();
	}

	free(): TVarSet {
		return this._map.values()
			.map(t => t.free())
			.reduce((x, y) => x.union(y), TVarSet.empty())
	}
	subst(sub: Subst): Subst {
		return new Subst(this._map.map(t => t.subst(sub)));
	}

	has(tv: TVar) {
		return this._map.has(tv.hash());
	}
	get(tv: TVar) {
		return this._map.get(tv.hash());
	}
	getOr(tv: TVar, t: Type) {
		return this._map.getOr(tv.hash(), t);
	}
	getMap<R>(key: TVar, map: (type: Type) => R, def: R) {
		return this._map.getMap(key.hash(), map, def);
	}

	compose(next: Subst): Subst {
		return new Subst(this._map.union(next._map.map(t => t.subst(this))));
	}

	removeTVars(tvars: TVarSet) {
		return new Subst(this._map.removeKeys(tvars.ids()));
	}
}