import { Kind, kfuns } from './kinds';
import { Type, tapps, tfuns, tforalls, tcon, tvar } from './types';
import { Expr } from './exprs';
import { ktype } from './typechecker';

export abstract class Definition {
  abstract toString(): string;
}

export class DValue extends Definition {
  constructor(public readonly name: string, public readonly val: Expr, public readonly type?: Type) { super() }

  toString(): string {
    return `${this.name}${this.type? ` : ${this.type}`: ''} = ${this.val}`;
  }
}

export class DData extends Definition {
  constructor(
    public readonly name: string,
    public readonly params: [string, Kind][],
    public readonly constrs: [string, Type[]][] 
  ) { super() }

  toString(): string {
    return `data ${this.name} ${this.params.length === 0? '': this.params.map(([x, k]) => `(${x} : ${k})`).join(' ')}${this.constrs.length === 0? '': ` = ${this.constrs.map(([x, ts]) => `${x}${ts.length === 0? '': ts.join(' ')}`).join(' | ')}`}`;
  }

  getKind(): Kind {
    return kfuns.apply(null, this.params.map(([n, k]) => k).concat([ktype]));
  }
  getType(): Type {
    return tapps.apply(null, [tcon(this.name) as Type].concat(this.params.map(([x, _]) => tvar(x))));
  }
}
