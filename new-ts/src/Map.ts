export type Hashable = { id: string };

type IMap<V> = { [key: string]: V };

export type Set<T extends Hashable> = Map<T, T>;

function contains<T>(arr: T[], val: T, eq: (a: T, b: T) => boolean) {
	for(let i = 0, l = arr.length; i < l; i++)
		if(eq(arr[i], val)) return true;
	return false;
}

export default class Map<K extends Hashable, V> {
	private readonly _keys: IMap<K>
	private readonly _vals: IMap<V>;

	private constructor(_keys: IMap<K>, _vals: IMap<V>) {
		this._keys = _keys;
		this._vals = _vals;
	}

	public static empty<K extends Hashable, V>() {
		return new Map<K, V>({}, {});
	}
	public static from<K extends Hashable, V>(kvs: [K, V][]) {
		const keys: IMap<K> = {};
		const vals: IMap<V> = {};
		for(let i = 0, l = kvs.length; i < l; i++) {
			const key = kvs[i][0];
			const val = kvs[i][1];
			keys[key.id] = key;
			vals[key.id] = val;
		}
		return new Map<K, V>(keys, vals);
	}
	public static of<K extends Hashable, V>(...kvs: [K, V][]) {
		return Map.from(kvs);
	}

	public ids() {
		return Object.keys(this._keys);
	}
	public keys() {
		return this.ids().map(k => this._keys[k]);
	}
	public values() {
		return this.ids().map(k => this._vals[k]);
	}

	public isEmpty() {
		return this.ids().length === 0;
	}

	public has(key: K) {
		return this._keys[key.id]? true: false;
	}
	public get(key: K): V | null {
		return this._vals[key.id] || null;
	}
	public getOr(key: K, def: V) {
		return this._vals[key.id] || def;
	}
	public getMap<R>(key: K, fn: (val: V) => R, def: R) {
		const v = this._vals[key.id];
		return v? fn(v): def;
	}

	public toString() {
		return `{${this.ids().map(k => `${k}: ${this._vals[k]}`).join(', ')}}`;
	}
	public toStringFormat(formatKey: (key: K) => string, formatVal: (val: V) => string) {
		return `{${this.ids().map(k => `${formatKey(this._keys[k])}: ${formatVal(this._vals[k])}`).join(', ')}}`;
	}
	public toStringFormatKey(formatKey: (key: K) => string) {
		return this.toStringFormat(formatKey, x => x.toString());
	}
	public toStringFormatValue(formatVal: (val: V) => string) {
		return this.toStringFormat(x => x.id, formatVal);
	}

	public map<R>(fn: (val: V, key: K) => R) {
		const vals: IMap<R> = {};
		for(let k in this._keys)
			vals[k] = fn(this._vals[k], this._keys[k]);
		return new Map<K, R>(this._keys, vals);
	}
	public filter(fn: (val: V, key: K) => boolean) {
		const keys: IMap<K> = {};
		const vals: IMap<V> = {};
		for(let k in this._keys) {
			const key = this._keys[k];
			const val = this._vals[k];
			if(fn(val, key)) {
				keys[k] = key;
				vals[k] = val;
			}
		}
		return new Map<K, V>(keys, vals);
	}

	public union(other: Map<K, V>) {
		const keys: IMap<K> = {};
		const vals: IMap<V> = {};
		for(let k in this._keys) {
			keys[k] = this._keys[k];
			vals[k] = this._vals[k];
		}
		for(let k in other._keys) {
			keys[k] = other._keys[k];
			vals[k] = other._vals[k];
		}
		return new Map<K, V>(keys, vals);
	}
	public pickFrom(ks: K[]) {
		const keys: IMap<K> = {};
		const vals: IMap<V> = {};
		for(let k in this._keys) {
			const key = this._keys[k];
			if(contains(ks, key, (a, b) => a.id === b.id)) {
				keys[k] = key;
				vals[k] = this._vals[k];
			}
		}
		return new Map<K, V>(keys, vals);
	}
	public pick(...ks: K[]) {
		return this.pickFrom(ks);
	}
	public intersection(other: Map<K, V>) {
		return this.pickFrom(other.keys());
	}
	public difference(other: Map<K, V>) {
		const keys: IMap<K> = {};
		const vals: IMap<V> = {};
		for(let k in this._keys) {
			if(!other._keys[k]) {
				keys[k] = this._keys[k];
				vals[k] = this._vals[k];
			}
		}
		for(let k in other._keys) {
			if(!this._keys[k]) {
				keys[k] = other._keys[k];
				vals[k] = other._vals[k];
			}
		}
		return new Map<K, V>(keys, vals);
	}

	public removeFrom(ks: K[]) {
		const keys: IMap<K> = {};
		const vals: IMap<V> = {};
		for(let k in this._keys) {
			const key = this._keys[k];
			if(!contains(ks, key, (a, b) => a.id === b.id)) {
				keys[k] = key;
				vals[k] = this._vals[k];
			}
		}
		return new Map<K, V>(keys, vals);
	}
	public remove(...ks: K[]) {
		return this.removeFrom(ks);
	}
	public without(other: Map<K, V>) {
		return this.removeFrom(other.keys());
	}
}
