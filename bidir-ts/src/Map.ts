export type Map<T> = { [key: string]: T };
export type KVPair<T> = [string, T];

export function mapFrom<T>(pairs: KVPair<T>[]): Map<T> {
	const r: Map<T> = {};
	for(let i = 0, l = pairs.length; i < l; i++) r[pairs[i][0]] = pairs[i][1];
	return r;
}

export function map<T>(...pairs: KVPair<T>[]): Map<T> {
	return mapFrom(pairs);
}

export function setFrom<T>(vals: T[]): Map<T> {
	return mapFrom(vals.map(x => ['' + x, x] as [string, T]));
}

export function set<T>(...vals: T[]): Map<T> {
	return setFrom(vals);
}

function clone<T>(o: {[key: string]: T}): {[key: string]: T} {
	let r: {[key: string]: T} = {};
	for(var k in o) r[k] = o[k];
	return r;
}

export function union<T>(a: Map<T>, b: Map<T>): Map<T> {
	const r: Map<T> = clone(a);
	for(let k in b) r[k] = b[k];
	return r;
}

export function remove<T>(map: Map<T>, key: string): Map<T> {
	let r = clone(map);
	delete r[key];
	return r;
}
