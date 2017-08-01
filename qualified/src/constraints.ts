import HasTVars from './HasTVars';
import TVarSet from './TVarSet';
import Subst from './Subst';

export abstract class Constraint implements HasTVars<Constraint> {
	abstract toString(): string;
	abstract free(): TVarSet;
	abstract subst(sub: Subst): Constraint;
}
