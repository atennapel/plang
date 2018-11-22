import { ValType } from './types';
import NameRep from '../NameRep';

export default abstract class Expr {

  abstract toString(): string;

  abstract subst(name: NameRep, val: Expr): Expr;
  
  abstract substTVar(name: NameRep, type: ValType): Expr;

}
