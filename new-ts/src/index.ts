import {
	Type,

	tunit,
	tvar,
	texists,
	tfuns,
	tforalls,

	typeStr,
} from './types';

const type = tforalls(['a', 'b'], tfuns([tunit, tvar('a'), tfuns([tunit, tunit]), tforalls(['c'], tvar('c')), texists('b')]));
console.log(typeStr(type));
