import Map from './Map';
import TVarSet from './TVarSet';
import { Scheme } from './types';
import HasTVars from './HasTVars';
import Subst from './Subst';

export default class Env implements HasTVars<Env> {
	private readonly _map: Map<Scheme>;

	constructor(_map: Map<Scheme>) {
		this._map = _map;
	}

	static from(vals: [string, Scheme][]) {
		return new Env(Map.from(vals));
	}
	static of(...vals: [string, Scheme][]) {
		return this.from(vals);
	}
	static empty() {
		return new Env(Map.empty());
	}

	has(key: string) {
		return this._map.has(key);
	}
	get(key: string) {
		return this._map.get(key);
	}
	getOr(key: string, val: Scheme) {
		return this._map.getOr(key, val);
	}
	getMap<R>(key: string, map: (val: Scheme) => R, val: R) {
		return this._map.getMap(key, map, val);
	}

	add(name: string, sch: Scheme) {
		return new Env(this._map.add(name, sch));
	}

	free(): TVarSet {
		return this._map.values().map(s => s.free()).reduce((a, b) => a.union(b), TVarSet.empty());
	}
	subst(sub: Subst): Env {
		return new Env(this._map.map(s => s.subst(sub)));
	}

	toString() {
		return this._map.toString();
	}
}
