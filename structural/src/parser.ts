import {
	Expr,
	EVar,
	evar,
	eapp,
	elam,
	elet,
	eletr,
	erecordempty,
	erecordselect,
	erecordextend,
	erecordrestrict,
	evariantinject,
	evariantembed,
	evariantelim,
} from './exprs';
import { Result } from './Result';

function makeapp(a: Expr[]): Expr {
	const f = a[0];
	if(f instanceof EVar) {
		if(f.name[0] === '\\')
			return elam(f.name.slice(1).split(''), makeapp(a.slice(1)));
		if(f.name.slice(0, 2) === '::')
			return eletr(f.name.slice(2), a[1], makeapp(a.slice(2)));
		if(f.name[0] === ':')
			return elet(f.name.slice(1), a[1], makeapp(a.slice(2)));
	}
	return a.length === 0? erecordempty:
				 a.length === 1? a[0]:
				 eapp.apply(null, a);
}

function makevar(s: string) {
	if(s.slice(0, 2) === '.+') return erecordextend(s.slice(2));
	if(s.slice(0, 2) === '.-') return erecordrestrict(s.slice(2));
	if(s[0] === '.') return erecordselect(s.slice(1));
	if(s.slice(0, 2) === '@+') return evariantembed(s.slice(2));
	if(s[0] === '@') return evariantinject(s.slice(1));
	if(s[0] === '?') return evariantelim(s.slice(1));
	return evar(s);
}

const START = 0;
const NAME = 1;
export default function parse(str: string): Result<SyntaxError, Expr> {
	let state = START;
	let t = '', level = 0;
	let r: Expr[] = [], p: Expr[][] = [];
	for(let i = 0, l = str.length; i <= l; i++) {
		const c = str[i] || ' ';
		if(state === START) {
			if(c === '(') p.push(r), r = [], level++;
			else if(c === ')') {
				if(level <= 0) return Result.err(new SyntaxError('Unmatched parens'));
				level -= 1;
				const e = makeapp(r);
				const rtemp = p.pop();
				if(rtemp) r = rtemp;
				else return Result.err(new SyntaxError('Something went wrong with parsing'));
				r.push(e);
			} else if(/[^\s\(\)]+/i.test(c)) t += c, state = NAME;
		} else if(state === NAME) {
			if(/[^\s\(\)]+/i.test(c)) t += c;
			else i--, r.push(makevar(t)), t = '', state = START;
		}
	}
	if(level !== 0)
		return Result.err(new SyntaxError('Unmatched parens'));
	return Result.ok(makeapp(r));
}
