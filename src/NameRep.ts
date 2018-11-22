export default abstract class NameRep {

  abstract name: string;

  abstract next(): NameRep;

  abstract withIndex(index: number): NameRep;

  abstract equals(other: NameRep): boolean;

  abstract toString(): string;

}

export class Name extends NameRep {

  constructor(
    public readonly name: string,
  ) { super() }

  next() {
    return new Gen(this.name, 0);
  }

  withIndex(index: number): NameRep {
    return new Gen(this.name, index);
  }

  equals(other: NameRep): boolean {
    return other instanceof Name && other.name === this.name;
  }

  toString() {
    return this.name;
  }

}
export const name = (name: string) => new Name(name);

export class Gen extends NameRep {
  
  constructor(
    public readonly name: string,
    public readonly index: number,
  ) { super() }

  next() {
    return new Gen(this.name, this.index + 1);
  }

  withIndex(index: number): NameRep {
    return new Gen(this.name, index);
  }

  equals(other: NameRep): boolean {
    return other instanceof Gen && other.name === this.name && other.index === this.index;
  }

  toString() {
    return `${this.name}\$${this.index}`;
  }

}
export const gen = (name: string, index: number) => new Gen(name, index);
