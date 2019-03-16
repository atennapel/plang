import { NameT, Gen } from './names'; 

export class NameStore {

  constructor(
    private readonly map: Map<string, number> = new Map(),
  ) {}

  toString(): string {
    const r: string[] = [];
    for (let [k, v] of this.map.entries()) r.push(`${k}: ${v}`);
    return `{${r.join(', ')}}`;
  }

  fresh(name_: string | NameT): Gen {
    const name = typeof name_ === 'string' ? name_ : name_.name;
    const id = this.map.get(name) || 0;
    this.map.set(name, id + 1);
    return Gen(name, id);
  }

}
