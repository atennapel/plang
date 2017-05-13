import Map, { KVPair } from './Map';
import { Hashable } from './Hashable';

export default class Set<T extends Hashable> {
  map: Map<T, T>;
  constructor(map: Map<T, T>) {
    this.map = map;
  }

  static from<T extends Hashable>(args: T[]) {
    return new Set<T>(Map.from<T, T>(args.map(x => <KVPair<T, T>> [x, x])));
  }
  static of<T extends Hashable>(...args: T[]) {
    return Set.from<T>(args);
  }
  static empty<T extends Hashable>(): Set<T> { return Set.of<T>() }

	size(): number { return this.map.size() }
	isEmpty(): boolean { return this.map.isEmpty() }

  keys(): string[] { return this.map.keys() }
  vals(): T[] { return this.map.vals() }

  toString(): string { return '{' + this.keys().join(', ') + '}' }

  contains(v: T): boolean { return !!this.map.contains(v) }

  union(other: Set<T>): Set<T> {
    return new Set(this.map.union(other.map));
  }
  without(other: Set<T>): Set<T> {
    return new Set(this.map.removeKeySet(other));
  }
	intersection(other: Set<T>): Set<T> {
		return new Set(this.map.intersection(other.map));
	}
}
