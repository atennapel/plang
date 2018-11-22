import NameRep, { Gen } from './NameRep';

export default class NameRepSupply {

  constructor(
    public readonly index: number,
  ) { }

  fresh(name: NameRep): { name: NameRep, supply: NameRepSupply } {
    return { name: new Gen(name.name, this.index), supply: new NameRepSupply(this.index + 1) };
  }

}
