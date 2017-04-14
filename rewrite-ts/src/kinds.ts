import { Eq } from './interfaces';
import { terr } from './utils';

export abstract class Kind implements Eq {
  constructor() {}
  eq(other: any): boolean { return false }
}

export class KCon extends Kind implements Eq {
  name: string;
  constructor(name: string) {
    super();
    this.name = name;
  }
  eq(other: KCon): boolean { return this.name === other.name }
  toString(): string { return this.name }
}

export class KArr extends Kind implements Eq {
  left: Kind;
  right: Kind;
  constructor(left: Kind, right: Kind) {
    super();
    this.left = left;
    this.right = right;
  }
  eq(other: KArr): boolean {
    return this.left.eq(other.left) && this.right.eq(other.right);
  }
  toString(): string {
    return '(' + this.left + ' -> ' + this.right + ')';
  }
}
export function karr(...args: Kind[]): Kind {
  if(args.length < 2) terr('karr needs at least 2 arguments');
  return args.reduceRight((l, r) => new KArr(r, l));
}

export let KType = new KCon('Type');
export let KRow = new KCon('Row');
