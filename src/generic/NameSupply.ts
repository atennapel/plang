import NameRep, { Gen } from './NameRep';

export interface INameSupply<T, N, S> {
  fresh(val: T): { name: N, supply: S };
}

export default class NameRepSupply implements INameSupply<NameRep, NameRep, NameRepSupply> {

  constructor(
    public readonly index: number,
  ) { }

  fresh(name: NameRep): { name: NameRep, supply: NameRepSupply } {
    return { name: new Gen(name.name, this.index), supply: new NameRepSupply(this.index + 1) };
  }

}
