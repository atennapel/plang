import { Expr, eapp, etapp, evar, EVar, eabss, etabss, eanno } from './exprs';
import { Kind, kfuns, kcon } from './kinds';
import { Type, tcon, tvar, tapps, tforalls, tfuns } from './types'
import { ktype } from './typechecker';

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

function tokenize(s: string): Ret[] {
  const START = 0, NAME = 1;
  let state = START;
  let r: Ret[] = [], p: Ret[][] = [], b: string[] = [];
  let t = '';
  for(let i = 0; i <= s.length; i++) {
    const c = s[i] || ' ';
    if(state === START) {
      if(/[a-z0-9]/i.test(c)) t += c, state = NAME;
      else if(c === '-' && s[i+1] === '>') r.push(token('->')), i++;
      else if(c === '/' && s[i+1] === '\\') r.push(token('/\\')), i++;
      else if(c === '@') r.push(token('@'));
      else if(c === ':') r.push(token(':'));
      else if(c === '.') r.push(token('.'));
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
      if(!/[a-z0-9\']/i.test(c)) r.push(token(t)), t = '', i--, state = START;
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

function exprs(x: Ret[]): Expr {
  if(x.length === 0) return evar('unit');
  if(x.length === 1) return expr(x[0]);
  if(containsToken(x, ':')) {
    const s = splitOn(x, x => isToken(x, ':'));
    if(s.length !== 2) throw new SyntaxError('nested anno :');
    const l = exprs(s[0]);
    const r = types(s[1]);
    return eanno(l, r);
  }
  if(isToken(x[0], '\\')) {
    const args: any[] = [];
    let found = -1;
    for(let i = 1; i < x.length; i++) {
      const c = x[i];
      if(isToken(c, '->')) {
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
        const r = types(s[1]);
        l.forEach(n => args.push([n, r]));
      } else throw new SyntaxError(`invalid arg: ${c}`);
    }
    if(found < 0) throw new SyntaxError(`missing -> after \\`);
    const rest = x.slice(found + 1);
    if(rest.length === 0) throw new SyntaxError(`missing body in function`);
    return eabss(args, exprs(rest));
  }
  if(isToken(x[0], '/\\')) {
    const args: any[] = [];
    let found = -1;
    for(let i = 1; i < x.length; i++) {
      const c = x[i];
      if(isToken(c, '->')) {
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
      } else throw new SyntaxError(`invalid arg to tabs: ${c}`);
    }
    if(found < 0) throw new SyntaxError(`missing -> after /\\`);
    const rest = x.slice(found + 1);
    if(rest.length === 0) throw new SyntaxError(`missing body in tabs`);
    return etabss(args.map(x => typeof x === 'string'? [x, ktype] as [string, Kind]: x), exprs(rest));
  }
  if(isToken(x[0], '@')) throw new SyntaxError('beginning @');
  let r = expr(x[0]);
  let next = false;
  for(let i = 1; i < x.length; i++) {
    const c = x[i];
    if(isToken(c, '@')) next = true;
    else if(next) {
      next = false;
      r = etapp(r, type(c));
    } else {
      r = eapp(r, expr(c));
    }
  }
  if(next) throw new SyntaxError('trailing @');
  return r;
}

function expr(x: Ret): Expr {
  if(x.tag === 'token') {
    const n = +x.val;
    if(!isNaN(n) && n >= 0) {
      let t: Expr = evar('z');
      for(let i = 0; i < n; i++) {
        t = eapp(evar('s'), t);
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
    if(found < 0) throw new SyntaxError(`missing -> after forall`);
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
