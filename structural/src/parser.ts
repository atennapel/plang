import {
	Expr,
	EVar,
	ENumber,
	evar,
	eapp,
	elam,
	elet,
	eletr,
	edo,
	erecordempty,
	erecordselect,
	erecordextend,
	erecordrestrict,
	erecordupdate,
	evariantinject,
	evariantembed,
	evariantelim,
	evariantupdate,
	eeffembed,
	ehandle,
	enumber,
	estring,
	eperform,
} from './exprs';
import { Result, Ok, Err } from './Result';

function split(a: any[], v: any): any[][] {
	const i = a.indexOf(v);
	if(i >= 0) {
		const first = a.slice(0, i);
		const rest = split(a.slice(i + 1), v);
		return [first].concat(rest);
	}
	return [a];
}

function makeapp(inp: Expr[]): Expr {
	const s = split(inp, DOLLAR) as Expr[][];
	if(s.length === 0) return erecordempty;
	if(s.length === 1) {
		const a = s[0];
		const f = a[0];
		if(f instanceof EVar) {
			if(f.name[0] === '\\')
				return elam(f.name.slice(1).split(''), makeapp(a.slice(1)));
			if(f.name.slice(0, 2) === '::')
				return eletr(f.name.slice(2), a[1], makeapp(a.slice(2)));
			if(f.name[0] === ':')
				return elet(f.name.slice(1), a[1], makeapp(a.slice(2)));
			if(f.name[0] === '^')
				return edo(f.name.slice(1), a[1], makeapp(a.slice(2)));
			if(f.name === 'handle') {
				const map = a[1];
				if(map && (map as any)._rec) {
					const rec = (map as any)._rec;
					let rr: [string, Expr][] = [];
					for(let i = 0, l = rec.length; i < l; i += 2) {
						rr.push([rec[i].name, rec[i + 1]]);
					}
					return eapp.apply(null, [ehandle(rr) as Expr].concat(a.slice(2)));
				} else throw Error('Invalid handle');
			}
		}
		return a.length === 0? erecordempty:
					a.length === 1? a[0]:
					eapp.apply(null, a);
	}
	return s.map(makeapp).reduceRight((a, b) => makeapp([b, a]));
}

function makevar(s: string) {
	const n = +s;
	if(s[0] !== '.' && !isNaN(n)) return enumber(n);
	if(s.slice(0, 2) === '.:') return erecordupdate(s.slice(2));
	if(s.slice(0, 2) === '.+') return erecordextend(s.slice(2));
	if(s.slice(0, 2) === '.-') return erecordrestrict(s.slice(2));
	if(s[0] === '.') return erecordselect(s.slice(1));
	if(s.slice(0, 2) === '@+') return evariantembed(s.slice(2));
	if(s.slice(0, 2) === '@:') return evariantupdate(s.slice(2));
	if(s[0] === '@') return evariantinject(s.slice(1));
	if(s[0] === '?') return evariantelim(s.slice(1));
	if(s.slice(0, 2) === '!+') return eeffembed(s.slice(2));
	if(s[0] === '!') return eperform(s.slice(1));
	return evar(s);
}

function makerec(a: Expr[]): Result<SyntaxError, Expr> {
	if(a.length % 2 !== 0) return Result.err(new SyntaxError('invalid record, odd number of members'));
	let r: Expr = erecordempty;
	for(let i = a.length - 1; i >= 0; i -= 2) {
		const k = a[i-1], v = a[i];
		if(k instanceof EVar) {
			r = eapp(erecordextend(k.name), v, r);		
		} else if(k instanceof ENumber) {
			r = eapp(erecordextend(k.val.toString()), v, r);		
		} else return Result.err(new SyntaxError('invalid key in record: ' + k));
	}
	(r as any)._rec = a;
	return Result.ok(r);
}

const DOLLAR = {toString: () => '$'};

function matchingBracket(b: string) {
	if(b === '(') return ')';
	if(b === ')') return '(';
	if(b === '{') return '}';
	if(b === '}') return '{';
	throw new Error('invalid bracket: ' + b);
}

const START = 0;
const NAME = 1;
const STRING = 2;
const COMMENT = 3;
export default function parse(str: string): Result<SyntaxError, Expr> {
	let state = START;
	let t = '';
	let r: Expr[] = [], p: Expr[][] = [], bs: string[] = [];
	for(let i = 0, l = str.length; i <= l; i++) {
		const c = str[i] || ' ';
		if(state === START) {
			if(c === ';') state = COMMENT;
			else if(c === '$') r.push(DOLLAR as Expr);
			else if(c === '"') state = STRING;
			else if(c === '(' || c === '{') p.push(r), r = [], bs.push(c);
			else if(c === ')' || c === '}') {
				if(bs.length <= 0) return Result.err(new SyntaxError('Unmatched parens: ' + c));
				const cb = bs.pop();
				if(!cb || matchingBracket(cb) !== c) return Result.err(new SyntaxError('Unmatched bracket: ' + cb + ' and ' + c));
				let e: Expr | null = null;
				if(c === ')')
					e = makeapp(r);
				else if(c === '}') {
					const rr = makerec(r);
					if(rr instanceof Err) return rr;
					if(rr instanceof Ok) e = rr.val;
				}
				const rtemp = p.pop();
				if(rtemp) r = rtemp;
				else return Result.err(new SyntaxError('Something went wrong with parsing'));
				if(e) r.push(e);
			} else if(/[^\s\(\){}]+/i.test(c)) t += c, state = NAME;
		} else if(state === NAME) {
			if(/[^\s\(\){}]+/i.test(c)) t += c;
			else i--, r.push(makevar(t)), t = '', state = START;
		} else if(state === STRING) {
			if(c === '"') r.push(estring(t)), t = '', state = START;
			else t += c;
		} else if(state === COMMENT) {
			if(c === '\n') state = START;
		}
	}
	if(bs.length !== 0)
		return Result.err(new SyntaxError('Unmatched parens: ' + bs.join(' ')));
	return Result.ok(makeapp(r));
}
