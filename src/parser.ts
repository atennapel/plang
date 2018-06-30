import {
  Expr,
  eapp,
  etapp,
  evar,
  eabss,
  etabss,
  eanno,
  elit,
  eempty,
  eselect,
  eextend,
  erestrict,
  erecupdate,
  evarempty,
  einject,
  eembed,
  ecase,
  evarupdate,
  ereturn,
  epure,
  eop,
  edo,
  ehandler,
} from './exprs';
import { Kind, kfuns, kcon } from './kinds';
import { Type, tcon, tvar, tapps, tforalls, tfuns } from './types'
import { ktype } from './typechecker';
import { Definition, DValue, DData } from './definitions'; 
import { impossible } from './util';
import { FORALL } from './prettyprinter';

function matchingBracket(c: string) {
  if(c === '(') return ')';
  if(c === ')') return '(';
  return '';
}

type Ret = Token | Paren;
interface Token {
  tag: 'token';
  val: string;
}
interface Paren {
  tag: 'paren';
  val: Ret[];
}
const token = (val: string): Token => ({ tag: 'token', val });
const paren = (val: Ret[]): Paren => ({ tag: 'paren', val });

function showRet(x: Ret): string {
  return x.tag === 'token'? x.val: x.tag === 'paren'? `(${x.val.map(showRet).join(' ')})`: '';
}
function showRets(x: Ret[]): string {
  return `[${x.map(showRet).join(' ')}]`;
}

function tokenize(s: string): Ret[] {
  const START = 0, NAME = 1, STR = 2, NUM = 3;
  let state = START;
  let r: Ret[] = [], p: Ret[][] = [], b: string[] = [];
  let t = '';
  let escape = false;
  for(let i = 0; i <= s.length; i++) {
    const c = s[i] || ' ';
    if(state === START) {
      if(/[a-z]/i.test(c)) t += c, state = NAME;
      else if(/[0-9]/.test(c)) t += c, state = NUM;
      else if(c === '"') state = STR;
      else if(c === '-' && s[i+1] === '>') r.push(token('->')), i++;
      else if(c === '/' && s[i+1] === '\\') r.push(token('/\\')), i++;
      else if(c === '@') r.push(token('@'));
      else if(c === '$') r.push(token('$'));
      else if(c === ':') r.push(token(':'));
      else if(c === FORALL) r.push(token('forall'));
      else if(c === '.') r.push(token('.'));
      else if(c === '=') r.push(token('='));
      else if(c === '|') r.push(token('|'));
      else if(c === '\\') r.push(token('\\'));
      else if(c === '(') b.push(c), p.push(r), r = [];
      else if(c === ')') {
        if(b.length === 0) throw new SyntaxError(`unmatched bracket: ${c}`);
        const br = b.pop() as string;
        if(matchingBracket(br) !== c) throw new SyntaxError(`unmatched bracket: ${br} and ${c}`);
        const a: Ret[] = p.pop() as Ret[];
        a.push(paren(r));
        r = a;
      } else if(/\s+/.test(c)) continue;
      else throw new SyntaxError(`invalid char: ${c}`);
    } else if(state === NAME) {
      if(!/[a-z0-9]/i.test(c)) r.push(token(t)), t = '', i--, state = START;
      else t += c;
    } else if(state === NUM) {
      if(!/[0-9\.]/i.test(c)) r.push(token(t)), t = '', i--, state = START;
      else t += c;
    } else if(state === STR) {
      if(escape) t += c, escape = false;
      else if(c === '\\') escape = true; 
      else if(c === '"') r.push(token(`"${t}`)), t = '', state = START;
      else t += c;
    }
  }
  if(state !== START) throw new SyntaxError(`invalid parsing state: ${state}`);
  return r;
}

export function parse(s: string): Expr {
  return exprs(tokenize(s));
}

function isToken(x: Ret, n: string): boolean {
  return x.tag === 'token' && x.val === n;
}
function containsToken(x: Ret[], n: string): boolean {
  return x.filter(x => x.tag === 'token').map(x => x.val as string).indexOf(n) >= 0;
}
function splitOn(x: Ret[], f: (x:Ret) => boolean) {
  const r: Ret[][] = [];
  let c: Ret[] = [];
  for(let i = 0; i < x.length; i++) {
    if(f(x[i])) {
      r.push(c);
      c = [];
    } else c.push(x[i]);
  }
  r.push(c);
  return r;
}

function exprs(x: Ret[], stack: Expr | null = null, mode: string | null = null): Expr {
  if(x.length === 0) return stack || evar('Unit');
  if(containsToken(x, ':')) {
    const xs = splitOn(x, t => isToken(t, ':'));
    if(stack || mode || xs.length !== 2) throw new SyntaxError('invalid use of :');
    return eanno(exprs(xs[0]), types(xs[1]));
  }
  const head = x[0];
  if(isToken(head, ':')) throw new SyntaxError('invalid use of :');
  if(isToken(head, '@')) {
    if(mode || !stack) throw new SyntaxError('invalid use of @');
    return exprs(x.slice(1), stack, '@');
  }
  if(isToken(head, '$')) {
    if(mode || !stack) throw new SyntaxError('invalid use of $');
    return eapp(stack, exprs(x.slice(1)));
  }
  if(isToken(head, '\\')) {
    if(mode) throw new SyntaxError('\\ after @');
    const args: (string | [string, Type])[] = [];
    let found = -1;
    for(let i = 1; i < x.length; i++) {
      const c = x[i];
      if(isToken(c, '->')) {
        found = i;
        break;
      } else if(c.tag === 'token') args.push(c.val);
      else if(c.tag === 'paren' && c.val.length === 0) args.push(['_', type(c)]);
      else if(c.tag === 'paren' && containsToken(c.val, ':')) {
        const s = splitOn(c.val, x => isToken(x, ':'));
        if(s.length !== 2) throw new SyntaxError('nested anno arg :');
        const l = s[0].map(x => {
          if(x.tag === 'token') return x.val;
          throw new SyntaxError(`invalid arg: ${x}`);
        });
        const r = types(s[1]);
        l.forEach(n => args.push([n, r]));
      } else throw new SyntaxError(`invalid arg: ${c}`);
    }
    if(found < 0) throw new SyntaxError(`missing -> after \\`);
    const rest = x.slice(found + 1);
    if(rest.length === 0) throw new SyntaxError(`missing body in function`);
    const abs = eabss(args, exprs(rest));
    return stack? eapp(stack, abs): abs;
  }
  if(isToken(x[0], '/\\')) {
    const args: [string, Kind][] = [];
    let found = -1;
    for(let i = 1; i < x.length; i++) {
      const c = x[i];
      if(isToken(c, '->')) {
        found = i;
        break;
      } else if(c.tag === 'token') args.push([c.val, ktype]);
      else if(c.tag === 'paren' && containsToken(c.val, ':')) {
        const s = splitOn(c.val, x => isToken(x, ':'));
        if(s.length !== 2) throw new SyntaxError('nested anno arg :');
        const l = s[0].map(x => {
          if(x.tag === 'token') return x.val;
          throw new SyntaxError(`invalid arg: ${x}`);
        });
        const r = kinds(s[1]);
        l.forEach(n => args.push([n, r]));
      } else throw new SyntaxError(`invalid arg to tabs: ${c}`);
    }
    if(found < 0) throw new SyntaxError(`missing -> after /\\`);
    const rest = x.slice(found + 1);
    if(rest.length === 0) throw new SyntaxError(`missing body in tabs`);
    const abs = etabss(args, exprs(rest));
    return stack? eapp(stack, abs): abs;
  }
  if(isToken(x[0], 'handler')) {
    const args: [string, Expr][] = [];
    for(let i = 1; i < x.length; i += 2) {
      const op = x[i];
      if(op.tag !== 'token') throw new SyntaxError(`invalid op in handler`);
      const e = expr(x[i+1]);
      args.push([op.val, e]);
    }
    const handler = ehandler(args);
    return stack? eapp(stack, handler): handler;
  }
  if(stack) {
    if(mode === '@') {
      return exprs(x.slice(1), etapp(stack, type(head)), null);
    } else {
      return exprs(x.slice(1), eapp(stack, expr(head)), mode);
    }
  } else {
    return exprs(x.slice(1), expr(head), mode);
  }
}

function expr(x: Ret): Expr {
  if(x.tag === 'token') {
    if(x.val === 'empty') return eempty;
    if(x.val === 'varempty') return evarempty;
    if(x.val === 'return') return ereturn;
    if(x.val === 'pure') return epure;
    if(x.val === 'do') return edo;
    if(x.val.startsWith('ext') && x.val.length > 3) return eextend(x.val.slice(3));
    if(x.val.startsWith('sel') && x.val.length > 3) return eselect(x.val.slice(3));
    if(x.val.startsWith('res') && x.val.length > 3) return erestrict(x.val.slice(3));
    if(x.val.startsWith('rupd') && x.val.length > 4) return erecupdate(x.val.slice(4));
    if(x.val.startsWith('inj') && x.val.length > 3) return einject(x.val.slice(3));
    if(x.val.startsWith('emb') && x.val.length > 3) return eembed(x.val.slice(3));
    if(x.val.startsWith('cs') && x.val.length > 2) return ecase(x.val.slice(2));
    if(x.val.startsWith('vupd') && x.val.length > 4) return evarupdate(x.val.slice(4));
    if(x.val.startsWith('op') && x.val.length > 2) return eop(x.val.slice(2));
    if(x.val[0] === '"') return elit(x.val.slice(1));
    const n = +x.val;
    if(!isNaN(n)) {
      if(x.val.indexOf('.') >= 0) return elit(n);
      if(n < 0) throw new SyntaxError(`invalid nat: ${n}`);
      let t: Expr = evar('Z');
      for(let i = 0; i < n; i++) {
        t = eapp(evar('S'), t);
      }
      return t;
    } else return evar(x.val);
  }
  return exprs(x.val as any);
}

function types(x: Ret[]): Type {
  if(x.length === 0) return tcon('Unit');
  if(x.length === 1) return type(x[0]);
  if(isToken(x[0], 'forall')) {
    const args: any[] = [];
    let found = -1;
    for(let i = 1; i < x.length; i++) {
      const c = x[i];
      if(isToken(c, '.')) {
        found = i;
        break;
      } else if(c.tag === 'token') args.push(c.val);
      else if(c.tag === 'paren' && containsToken(c.val, ':')) {
        const s = splitOn(c.val, x => isToken(x, ':'));
        if(s.length !== 2) throw new SyntaxError('nested anno arg :');
        const l = s[0].map(x => {
          if(x.tag === 'token') return x.val;
          throw new SyntaxError(`invalid arg: ${x}`);
        });
        const r = kinds(s[1]);
        l.forEach(n => args.push([n, r]));
      } else throw new SyntaxError(`invalid arg to forall: ${c}`);
    }
    if(found < 0) throw new SyntaxError(`missing . after forall`);
    const rest = x.slice(found + 1);
    if(rest.length === 0) throw new SyntaxError(`missing body in forall`);
    return tforalls(args.map(x => typeof x === 'string'? [x, ktype] as [string, Kind]: x), types(rest));
  }
  if(containsToken(x, '->')) return tfuns.apply(null, splitOn(x, x => isToken(x, '->')).map(types));
  return tapps.apply(null, x.map(type));
}

function type(x: Ret): Type {
  if(x.tag === 'token') return /[a-z]/.test(x.val[0])? tvar(x.val): tcon(x.val);
  return types(x.val);
}

function kinds(x: Ret[]): Kind {
  if(x.length === 0) throw new SyntaxError('kind () is not allowed');
  if(x.length === 1) return kind(x[0]);
  const t = splitOn(x, x => isToken(x, '->')).map(kinds);
  if(t.length === 0) throw new SyntaxError(`invalid kind syntax`);
  return kfuns.apply(null, t);
}

function kind(x: Ret): Kind {
  if(x.tag === 'token') return kcon(x.val);
  return kinds(x.val);
}

// definitions
function parseDataName(s: Ret[]): [string, [string, Kind][]] {
  if(s.length === 0) throw new SyntaxError('missing data name');
  if(s[0].tag !== 'token') throw new SyntaxError('invalid data name');
  const name = s[0].val as string;
  const args: any[] = [];
  for(let i = 1; i < s.length; i++) {
    const c = s[i];
    if(c.tag === 'token') args.push([c.val, ktype]);
    else if(c.tag === 'paren' && containsToken(c.val, ':')) {
      const s = splitOn(c.val, x => isToken(x, ':'));
      if(s.length !== 2) throw new SyntaxError('nested anno arg :');
      const l = s[0].map(x => {
        if(x.tag === 'token') return x.val;
        throw new SyntaxError(`invalid arg to data: ${x}`);
      });
      const r = kinds(s[1]);
      l.forEach(n => args.push([n, r]));
    } else throw new SyntaxError(`invalid arg to data: ${c}`);
  }
  return [name, args];
}

function parseConstr(s: Ret[]): [string, Type[]] {
  if(s.length === 0) throw new SyntaxError('missing constructor name in data');
  if(s[0].tag !== 'token') throw new SyntaxError('invalid data constructor name');
  const name = s[0].val as string;
  return [name, s.slice(1).map(type)];
}

export function parseDefinition(s: string): Definition {
  if(s.startsWith('data ')) {
    const ts = tokenize(s.slice(5));
    if(ts.length === 0) throw new SyntaxError('data name missing');
    if(ts[0].tag !== 'token') throw new SyntaxError('invalid data name');
    const name = ts[0].val as string;
    if(ts.length === 1) return new DData(name, [], []);
    if(containsToken(ts, '=')) {
      const spl = splitOn(ts, x => isToken(x, '='));
      if(spl.length !== 2) throw new SyntaxError('missing right side of = in data');
      const dataName = parseDataName(spl[0]);
      const constr = splitOn(spl[1], x => isToken(x, '|')).map(parseConstr);
      return new DData(dataName[0], dataName[1], constr);
    } else throw new SyntaxError('= is missing in data');
  } else {
    const spl = s.split('=');
    if(!spl || spl.length !== 2) throw new SyntaxError('error on =');
    const name = spl[0].trim();
    if(!/[a-z][A-Z0-9a-z]*/.test(name)) throw new SyntaxError(`invalid name: ${name}`);
    const rest = spl[1].trim();
    return new DValue(name, parse(rest));
  }
}

export function parseProgram(s: string): Definition[] {
  return s.split(';').filter(x => x.trim().length > 0).map(x => parseDefinition(x.trim()));
}
