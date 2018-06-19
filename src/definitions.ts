import { Kind, kfun } from './kinds';
import { Type, tapps, tfuns, tforalls, tcon, tvar } from './types';
import { Expr } from './exprs';
import { Context, ContextElem, ctcon, cvar } from './context';
import { ktype } from './typechecker';

export abstract class Definition {
  abstract toString(): string;
  abstract getContext(): Context;
}

export class DValue extends Definition {
  constructor(public readonly name: string, public readonly val: Expr) { super() }

  toString(): string {
    return `${this.name} = ${this.val}`;
  }

  getContext(): Context {
    return new Context([]);
  }
}

export class DAnno extends Definition {
  constructor(public readonly name: string, public readonly type: Type) { super() }

  toString(): string {
    return `${this.name} : ${this.type}`;
  }

  getContext(): Context {
    return new Context([cvar(this.name, this.type)]);
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
    return kfun.apply(null, this.params.map(([n, k]) => k).concat([ktype]));
  }

  getType(): Type {
    return tapps.apply(null, [tcon(this.name) as Type].concat(this.params.map(([x, _]) => tvar(x))));
  }

  getTCon(): ContextElem {
    return ctcon(this.name, this.getKind());
  }

  getConstructorDefs(): Definition[] {
    return this.constrs.map(([c, ts]) => [
      new DAnno(c, tforalls(this.params, tfuns.apply(null, ts.concat([this.getType()])))),
    ]).reduce((a, b) => a.concat(b), []);
  }

  getContext(): Context {
    return [new Context([this.getTCon()])].concat(this.getConstructorDefs().map(d => d.getContext())).reduce((a, b) => a.append(b), new Context([]));
  }
}
