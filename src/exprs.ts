import { Type } from './types';
import { Kind } from './kinds';

export abstract class Expr {
  abstract toString(): string;
  abstract subst(name: string, expr: Expr): Expr;
  abstract substType(name: string, type: Type): Expr;
}

export class ELit extends Expr {
  constructor(public readonly val: string | number) { super() }

  toString() {
    return typeof this.val === 'string'? JSON.stringify(this.val): `${this.val}`;
  }
  subst(name: string, expr: Expr): Expr {
    return this;
  }
  substType(name: string, type: Type): Expr {
    return this;
  }
}
export const elit = (val: string | number) => new ELit(val);

export class EVar extends Expr {
  constructor(public readonly name: string) { super() }

  toString() {
    return `${this.name}`;
  }
  subst(name: string, expr: Expr): Expr {
    return this.name === name? expr: this;
  }
  substType(name: string, type: Type): Expr {
    return this;
  }
}
export const evar = (name: string) => new EVar(name);

export class EApp extends Expr {
  constructor(
    public readonly left: Expr,
    public readonly right: Expr
  ) { super() }

  toString() {
    return `(${this.left} ${this.right})`;
  }
  subst(name: string, expr: Expr): Expr {
    return new EApp(this.left.subst(name, expr), this.right.subst(name, expr));
  }
  substType(name: string, type: Type): Expr {
    return new EApp(this.left.substType(name, type), this.right.substType(name, type));
  }
}
export const eapp = (left: Expr, right: Expr) => new EApp(left, right);
export const eapps = (...es: Expr[]) => es.reduce(eapp);

export class EAbs extends Expr {
  constructor(
    public readonly name: string,
    public readonly type: Type | null,
    public readonly expr: Expr
  ) { super() }

  isAnnotated() {
    return !!this.type;
  }

  toString() {
    return this.type? `(\\(${this.name} : ${this.type}) -> ${this.expr})`: `(\\${this.name} -> ${this.expr})`;
  }
  subst(name: string, expr: Expr): Expr {
    return this.name === name? this: new EAbs(this.name, this.type, this.expr.subst(name, expr));
  }
  open(expr: Expr): Expr {
    return this.expr.subst(this.name, expr);
  }
  substType(name: string, type: Type): Expr {
    return new EAbs(this.name, this.type && this.type.subst(name, type), this.expr.substType(name, type));
  }
}
export const eabs = (name: string, expr: Expr) => new EAbs(name, null, expr);
export const eabst = (name: string, type: Type, expr: Expr) => new EAbs(name, type, expr);
export const eabss = (ns: (string | [string, Type])[], expr: Expr) =>
  ns.reduceRight((a, b) => typeof b === 'string'? eabs(b, a): eabst(b[0], b[1], a), expr);

export class EAnno extends Expr {
  constructor(
    public readonly expr: Expr,
    public readonly type: Type
  ) { super() }

  toString() {
    return `(${this.expr} : ${this.type})`;
  }
  subst(name: string, expr: Expr): Expr {
    return new EAnno(this.expr.subst(name, expr), this.type);
  }
  substType(name: string, type: Type): Expr {
    return new EAnno(this.expr.substType(name, type), this.type.subst(name, type));
  }
}
export const eanno = (expr: Expr, type: Type) => new EAnno(expr, type);

export class ETAbs extends Expr {
  constructor(
    public readonly name: string,
    public readonly kind: Kind,
    public readonly expr: Expr
  ) { super() }

  toString() {
    return `(/\\(${this.name} : ${this.kind}) -> ${this.expr})`;
  }
  subst(name: string, expr: Expr): Expr {
    return new ETAbs(this.name, this.kind, this.expr.subst(name, expr));
  }
  substType(name: string, type: Type): Expr {
    return this.name === name? this: new ETAbs(this.name, this.kind, this.expr.substType(name, type));
  }
}
export const etabs = (name: string, kind: Kind, expr: Expr) => new ETAbs(name, kind, expr);
export const etabss = (ns: [string, Kind][], expr: Expr) =>
  ns.reduceRight((a, b) => etabs(b[0], b[1], a), expr);

export class ETApp extends Expr {
  constructor(
    public readonly expr: Expr,
    public readonly type: Type
  ) { super() }

  toString() {
    return `(${this.expr} @${this.type})`;
  }
  subst(name: string, expr: Expr): Expr {
    return new ETApp(this.expr.subst(name, expr), this.type);
  }
  substType(name: string, type: Type): Expr {
    return new ETApp(this.expr.substType(name, type), this.type.subst(name, type));
  }
}
export const etapp = (expr: Expr, type: Type) => new ETApp(expr, type);
export const etapps = (expr: Expr, ...ts: Type[]) => ts.reduce(etapp, expr);

export class EEmpty extends Expr {
  toString() {
    return `{}`;
  }
  subst(name: string, expr: Expr): Expr {
    return this;
  }
  substType(name: string, type: Type): Expr {
    return this;
  }
}
export const eempty = new EEmpty;

export class EExtend extends Expr {
  constructor(
    public readonly label: string,
  ) { super() }

  toString() {
    return `.+${this.label}`;
  }
  subst(name: string, expr: Expr): Expr {
    return this;
  }
  substType(name: string, type: Type): Expr {
    return this;
  }
}
export const eextend = (label: string) => new EExtend(label);

export const erecord = (props: [string, Expr][], rest?: Expr) =>
  props.reduceRight((r, [l, t]) => eapps(eextend(l), t, r), rest || eempty);

export class ESelect extends Expr {
  constructor(
    public readonly label: string,
  ) { super() }

  toString() {
    return `.${this.label}`;
  }
  subst(name: string, expr: Expr): Expr {
    return this;
  }
  substType(name: string, type: Type): Expr {
    return this;
  }
}
export const eselect = (label: string) => new ESelect(label);

export class ERestrict extends Expr {
  constructor(
    public readonly label: string,
  ) { super() }

  toString() {
    return `.-${this.label}`;
  }
  subst(name: string, expr: Expr): Expr {
    return this;
  }
  substType(name: string, type: Type): Expr {
    return this;
  }
}
export const erestrict = (label: string) => new ERestrict(label);

export class ERecSet extends Expr {
  constructor(
    public readonly label: string,
  ) { super() }

  toString() {
    return `.:=${this.label}`;
  }
  subst(name: string, expr: Expr): Expr {
    return this;
  }
  substType(name: string, type: Type): Expr {
    return this;
  }
}
export const erecset = (label: string) => new ERecSet(label);

export class ERecUpdate extends Expr {
  constructor(
    public readonly label: string,
  ) { super() }

  toString() {
    return `.:${this.label}`;
  }
  subst(name: string, expr: Expr): Expr {
    return this;
  }
  substType(name: string, type: Type): Expr {
    return this;
  }
}
export const erecupdate = (label: string) => new ERecUpdate(label);

export class EVarEmpty extends Expr {
  toString() {
    return `varEmpty`;
  }
  subst(name: string, expr: Expr): Expr {
    return this;
  }
  substType(name: string, type: Type): Expr {
    return this;
  }
}
export const evarempty = new EVarEmpty;

export class EInject extends Expr {
  constructor(
    public readonly label: string,
  ) { super() }

  toString() {
    return `!${this.label}`;
  }
  subst(name: string, expr: Expr): Expr {
    return this;
  }
  substType(name: string, type: Type): Expr {
    return this;
  }
}
export const einject = (label: string) => new EInject(label);

export class EEmbed extends Expr {
  constructor(
    public readonly label: string,
  ) { super() }

  toString() {
    return `!+${this.label}`;
  }
  subst(name: string, expr: Expr): Expr {
    return this;
  }
  substType(name: string, type: Type): Expr {
    return this;
  }
}
export const eembed = (label: string) => new EEmbed(label);

export class ECase extends Expr {
  constructor(
    public readonly label: string,
  ) { super() }

  toString() {
    return `?${this.label}`;
  }
  subst(name: string, expr: Expr): Expr {
    return this;
  }
  substType(name: string, type: Type): Expr {
    return this;
  }
}
export const ecase = (label: string) => new ECase(label);

export class EReturn extends Expr {
  toString() {
    return `return`;
  }
  subst(name: string, expr: Expr): Expr {
    return this;
  }
  substType(name: string, type: Type): Expr {
    return this;
  }
}
export const ereturn = new EReturn();

export class EPure extends Expr {
  toString() {
    return `pure`;
  }
  subst(name: string, expr: Expr): Expr {
    return this;
  }
  substType(name: string, type: Type): Expr {
    return this;
  }
}
export const epure = new EPure();

export class EOp extends Expr {
  constructor(
    public readonly label: string,
  ) { super() }

  toString() {
    return `(perform ${this.label})`;
  }
  subst(name: string, expr: Expr): Expr {
    return this;
  }
  substType(name: string, type: Type): Expr {
    return this;
  }
}
export const eop = (label: string) => new EOp(label);

export class EEffEmbed extends Expr {
  constructor(
    public readonly label: string,
  ) { super() }

  toString() {
    return `(effembed ${this.label})`;
  }
  subst(name: string, expr: Expr): Expr {
    return this;
  }
  substType(name: string, type: Type): Expr {
    return this;
  }
}
export const eeffembed = (label: string) => new EEffEmbed(label);

export class EDo extends Expr {
  toString() {
    return `do`;
  }
  subst(name: string, expr: Expr): Expr {
    return this;
  }
  substType(name: string, type: Type): Expr {
    return this;
  }
}
export const edo = new EDo();

export class EHandler extends Expr {
  constructor(
    public readonly map: [string, Expr][],
  ) { super() }

  toString() {
    return `(handler { ${this.map.map(([op, h]) => `${op} -> ${h}`).join(', ')} })`;
  }
  subst(name: string, expr: Expr): Expr {
    return new EHandler(this.map.map(([op, e]) => [op, e.subst(name, expr)] as [string, Expr]));
  }
  substType(name: string, type: Type): Expr {
    return new EHandler(this.map.map(([op, e]) => [op, e.substType(name, type)] as [string, Expr]));
  }
}
export const ehandler = (map: [string, Expr][]) => new EHandler(map);
