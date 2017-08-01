import { Result } from './Result';

export default class Map<T> {
	private readonly _map: {[key: string]: T};

	constructor(_map: {[key: string]: T}) {
		this._map = _map;
	}

	static from<T>(vals: [string, T][]) {
		const _map: {[key: string]: T} = {};
		for(let i = 0, l = vals.length; i < l; i++)
			_map[vals[i][0]] = vals[i][1];
		return new Map<T>(_map);
	}
	static of<T>(...vals: [string, T][]) {
		return Map.from(vals);
	}
	static empty<T>() {
		return new Map<T>({});
	}

	toString() {
		return `{${Object.keys(this._map).map(k => `${k}: ${this._map[k]}`).join(', ')}}`;
	}

	union(other: Map<T>) {
		const _map: {[key: string]: T} = {};
		for(let k in this._map) _map[k] = this._map[k];
		for(let k in other._map) _map[k] = other._map[k];
		return new Map<T>(_map);
	}

	has(key: string): boolean {
		return typeof this._map[key] !== 'undefined';
	}
	get(key: string): Result<Error, T> {
		return this.has(key)?
			Result.ok(this._map[key]):
			Result.err(new Error(`Map does not contain key: ${key}`));
	}
	getOr(key: string, val: T) {
		return this.has(key)? this._map[key]: val;
	}
	getMap<R>(key: string, map: (val: T) => R, val: R) {
		return this.has(key)? map(this._map[key]): val;
	}
	add(name: string, val: T) {
		const _map: {[key: string]: T} = {};
		for(let k in this._map) _map[k] = this._map[k];
		_map[name] = val;
		return new Map(_map);
	}

	keys(): string[] {
		return Object.keys(this._map);
	}
	values(): T[] {
		return this.keys().map(k => this._map[k]);
	}
	size() {
		return this.keys().length;
	}

	removeKeys(keys: string[]) {
		const _map: {[key: string]: T} = {};
		for(let k in this._map) {
			if(keys.indexOf(k) < 0) _map[k] = this._map[k];
		}
		return new Map(_map);
	}

	map<R>(fn: (val: T, key: string) => R): Map<R> {
		const _map: {[key: string]: R} = {};
		for(let k in this._map) _map[k] = fn(this._map[k], k);
		return new Map(_map);
	}
}
