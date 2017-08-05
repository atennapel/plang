import TVarSet from './TVarSet';
import Subst from './Subst';

export default interface HasTVars<T> {
	free(): TVarSet;
	subst(sub: Subst): T;
}
