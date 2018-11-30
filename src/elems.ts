import Kind from './kinds';
import Type from './types';
import NameRep from './NameRep';

export default abstract class Elem {

  abstract toString(): string;

}

export class CKVar extends Elem {

  constructor(
    public readonly name: NameRep,
  ) { super() }

  toString() {
    return `kind ${this.name}`;
  }

}
export const ckvar = (name: NameRep) => new CKVar(name);
export const isCKVar = (name: NameRep) =>
  (e: Elem): e is CKVar => e instanceof CKVar && e.name.equals(name);

export class CTVar extends Elem {

  constructor(
    public readonly name: NameRep,
    public readonly kind: Kind,
  ) { super() }

  toString() {
    return `${this.name} :k ${this.kind}`;
  }

}
export const ctvar = (name: NameRep, kind: Kind) => new CTVar(name, kind);
export const isCTVar = (name: NameRep) =>
  (e: Elem): e is CTVar => e instanceof CTVar && e.name.equals(name);

export class CTMeta extends Elem {

  constructor(
    public readonly name: NameRep,
    public readonly kind: Kind,
    public readonly type: Type | null,
  ) { super() }

  toString() {
    return this.type ? `^${this.name} :k ${this.kind} = ${this.type}` : `^${this.name} :k ${this.kind}`;
  }

  solve(type: Type) {
    return new CTMeta(this.name, this.kind, type);
  }

}
export const ctmeta = (name: NameRep, kind: Kind) => new CTMeta(name, kind, null);
export const csolved = (name: NameRep, kind: Kind, type: Type) => new CTMeta(name, kind, type);
export const isCTMeta = (name: NameRep) =>
  (e: Elem): e is CTMeta => e instanceof CTMeta && e.name.equals(name);

export class CMarker extends Elem {

  constructor(
    public readonly name: NameRep,
  ) { super() }

  toString() {
    return `|>${this.name}`;
  }

}
export const cmarker = (name: NameRep) => new CMarker(name);
export const isCMarker = (name: NameRep) =>
  (e: Elem): e is CMarker => e instanceof CMarker && e.name.equals(name);

export class CVar extends Elem {

  constructor(
    public readonly name: NameRep,
    public readonly type: Type,
  ) { super() }

  toString() {
    return `${this.name} : ${this.type}`;
  }

}
export const cvar = (name: NameRep, type: Type) => new CVar(name, type);
export const isCVar = (name: NameRep) =>
  (e: Elem): e is CVar => e instanceof CVar && e.name.equals(name);

export class CEff extends Elem {

  constructor(
    public readonly name: NameRep,
  ) { super() }

  toString() {
    return `eff ${this.name}`;
  }

}
export const ceff = (name: NameRep) => new CEff(name);
export const isCEff = (name: NameRep) =>
  (e: Elem): e is CEff => e instanceof CEff && e.name.equals(name);

export class COp extends Elem {

  constructor(
    public readonly name: NameRep,
    public readonly eff: NameRep,
    public readonly paramty: Type,
    public readonly returnty: Type,
  ) { super() }

  toString() {
    return `op ${this.name} of ${this.eff} : ${this.paramty} -> ${this.returnty}`;
  }

}
export const cop = (name: NameRep, eff: NameRep, paramty: Type, returnty: Type) =>
  new COp(name, eff, paramty, returnty);
export const isCOp = (name: NameRep) =>
  (e: Elem): e is COp => e instanceof COp && e.name.equals(name);
