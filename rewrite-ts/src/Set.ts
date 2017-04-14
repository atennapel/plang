import { Hash } from './interfaces';
import Map, { KVPair } from './Map';

export default class Set<T extends Hash> {
  map: Map<T, T>;
  constructor(map: Map<T, T>) {
    this.map = map;
  }

  static from<T extends Hash>(args: T[]) {
    return new Set<T>(Map.from<T, T>(args.map(x => <KVPair<T, T>> [x, x])));
  }
  static of<T extends Hash>(...args: T[]) {
    return Set.from<T>(args);
  }
  static empty<T extends Hash>(): Set<T> { return Set.of<T>() }

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
}
