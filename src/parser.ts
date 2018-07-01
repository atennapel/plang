import {
  Expr,
  eapp,
  eabs,
  eapps,
  etapp,
  evar,
  eabss,
  etabss,
  eanno,
  elit,
  eempty,
  eselect,
  eextend,
  erecset,
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
  erecord,
  EVar,
} from './exprs';
import { Kind, kfuns, kcon } from './kinds';
import { Type, tcon, tvar, tapps, tforalls, tfuns, trow, tempty } from './types'
import { ktype } from './typechecker';
import { Definition, DValue, DData } from './definitions';
import { FORALL } from './prettyprinter';

function matchingBracket(c: string) {
  if(c === '(') return ')';
  if(c === ')') return '(';
  if(c === '{') return '}';
  if(c === '}') return '{';
  if(c === '[') return ']';
  if(c === ']') return '[';
  return '';
}

type Ret = Token | Paren;
interface Token {
  tag: 'token';
  val: string;
}
interface Paren {
  tag: 'paren';
  type: string;
  val: Ret[];
}
const token = (val: string): Token => ({ tag: 'token', val });
const paren = (val: Ret[], type: string): Paren => ({ tag: 'paren', type, val });

function showRet(x: Ret): string {
  return x.tag === 'token'? x.val: x.tag === 'paren'? `${x.type}${x.val.map(showRet).join(' ')}${matchingBracket(x.type)}`: '';
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
      else if(c === '<' && s[i+1] === '-') r.push(token('<-')), i++;
      else if(c === '/' && s[i+1] === '\\') r.push(token('/\\')), i++;
      else if(c === ':' && s[i+1] === ':' && s[i+2] === '=') r.push(token('::=')), i += 2;
      else if(c === ':' && s[i+1] === '=') r.push(token(':=')), i++;
      else if(c === '@') r.push(token('@'));
      else if(c === '$') r.push(token('$'));
      else if(c === ':') r.push(token(':'));
      else if(c === ';') r.push(token(';'));
      else if(c === FORALL) r.push(token('forall'));
      else if(c === '.') r.push(token('.'));
      else if(c === '-') r.push(token('-'));
      else if(c === '#') r.push(token('#'));
      else if(c === '!') r.push(token('!'));
      else if(c === '_') r.push(token('_'));
      else if(c === '=') r.push(token('='));
      else if(c === '|') r.push(token('|'));
      else if(c === ',') r.push(token(','));
      else if(c === '\\') r.push(token('\\'));
      else if(c === '(' || c === '{' || c === '[') b.push(c), p.push(r), r = [];
      else if(c === ')' || c === '}' || c === ']') {
        if(b.length === 0) throw new SyntaxError(`unmatched bracket: ${c}`);
        const br = b.pop() as string;
        if(matchingBracket(br) !== c) throw new SyntaxError(`unmatched bracket: ${br} and ${c}`);
        const a: Ret[] = p.pop() as Ret[];
        a.push(paren(r, br));
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
function comesBefore(x: Ret[], a: string, b: string) {
  for(let i = 0; i < x.length; i++) {
    if(isToken(x[i], a))
      return true;
    else if(isToken(x[i], b))
      return false;
  }
  return false;
}

function exprs(x: Ret[], stack: Expr | null = null, mode: string | null = null): Expr {
  // console.log(`exprs ${showRets(x)} ${stack} ${mode}`);
  if(x.length === 0) {
    if(mode) throw new SyntaxError(`invalid use of ${mode}`);
    return stack || evar('Unit');
  }
  if(containsToken(x, ':')) {
    const xs = splitOn(x, t => isToken(t, ':'));
    if(stack || mode || xs.length !== 2) throw new SyntaxError('invalid use of :');
    return eanno(exprs(xs[0]), types(xs[1]));
  }
  if(containsToken(x, ';') && !comesBefore(x, '\\', ';') && !comesBefore(x, '/\\', ';')) {
    const xs = splitOn(x, t => isToken(t, ';'));
    if(stack || mode || xs.length < 2) throw new SyntaxError('invalid use of ;');
    const parts = xs.map((part, i, a) => {
      if(part.length === 0) throw new SyntaxError('empty ;');
      const fst = part[0];
      const snd = part[1];
      const last = i === a.length - 1;
      if(fst && snd && fst.tag === 'token' && snd.tag === 'token' && snd.val === '<-') {
        if(last) throw new SyntaxError('last part of ; cannot contain <-');
        return [fst.val, exprs(part.slice(2))] as [string, Expr];
      }
      return ['_', exprs(part)] as [string, Expr];
    });
    return parts.slice(0, -1).reduceRight((x, [n, e]) => eapps(edo, e, eabs(n, x)), parts[parts.length - 1][1] as Expr);
  }
  const head = x[0];
  if(isToken(head, ':')) throw new SyntaxError('invalid use of :');
  if(isToken(head, '@')) {
    if(mode || !stack) throw new SyntaxError('invalid use of @');
    return exprs(x.slice(1), stack, '@');
  }
  if(isToken(head, '.')) {
    if(mode) throw new SyntaxError('invalid use of $');
    return exprs(x.slice(1), stack, '.');
  }
  if(isToken(head, '#')) {
    if(mode || stack) throw new SyntaxError('invalid use of #');
    return exprs(x.slice(1), stack, '#');
  }
  if(isToken(head, '!')) {
    if(mode || stack) throw new SyntaxError('invalid use of !');
    return exprs(x.slice(1), stack, '!');
  }
  if(isToken(head, '$')) {
    if(mode) throw new SyntaxError('invalid use of $');
    if(!stack) return x.length < 2? evar('app'): eapps(evar('flip'), evar('app'), exprs(x.slice(1)));
    if(x.length < 2) throw new SyntaxError('invalid use of $');
    return eapp(stack as Expr, exprs(x.slice(1)));
  }
  if(isToken(head, '\\')) {
    if(mode) throw new SyntaxError(`\\ after ${mode}`);
    const args: (string | [string, Type])[] = [];
    let found = -1;
    for(let i = 1; i < x.length; i++) {
      const c = x[i];
      if(isToken(c, '->')) {
        found = i;
        break;
      } else if(c.tag === 'token') args.push(c.val);
      else if(c.tag === 'paren' && c.type !== '(') throw new SyntaxError(`unexpected bracket in \\ ${c.type}`);
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
    if(args.length === 0) throw new SyntaxError('\\ without args');
    if(rest.length === 0) throw new SyntaxError(`missing body in function`);
    const abs = eabss(args, exprs(rest));
    return stack? eapp(stack, abs): abs;
  }
  if(isToken(x[0], '/\\')) {
    if(mode) throw new SyntaxError(`/\\ after ${mode}`);
    const args: [string, Kind][] = [];
    let found = -1;
    for(let i = 1; i < x.length; i++) {
      const c = x[i];
      if(isToken(c, '->')) {
        found = i;
        break;
      } else if(c.tag === 'token') args.push([c.val, ktype]);
      else if(c.tag === 'paren' && c.type !== '(') throw new SyntaxError(`unexpected bracket in /\\ ${c.type}`);
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
    if(args.length === 0) throw new SyntaxError('/\\ without args');
    if(rest.length === 0) throw new SyntaxError(`missing body in tabs`);
    const abs = etabss(args, exprs(rest));
    return stack? eapp(stack, abs): abs;
  }
  if(isToken(x[0], 'handler')) {
    if(mode) throw new SyntaxError(`handler after ${mode}`);
    const args: [string, Expr][] = [];
    for(let i = 1; i < x.length; i += 2) {
      const op = x[i];
      if(op.tag !== 'token') throw new SyntaxError(`invalid op in handler`);
      const e = expr(x[i+1]);
      args.push([op.val, e]);
    }
    if(args.length === 0) throw new SyntaxError('empty handler');
    const handler = ehandler(args);
    return stack? eapp(stack, handler): handler;
  }
  if(mode === '.') {
    if(head.tag !== 'token') throw new SyntaxError(`invalid rhs to .`);
    const sel = stack? eapp(eselect(head.val), stack): eselect(head.val);
    return exprs(x.slice(1), sel, null);
  }
  if(mode === '#') {
    if(head.tag !== 'token') throw new SyntaxError(`invalid rhs to #`);
    return exprs(x.slice(1), einject(head.val), null);
  }
  if(mode === '!') {
    if(head.tag !== 'token') throw new SyntaxError(`invalid rhs to !`);
    return exprs(x.slice(1), eop(head.val), null);
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
    if(x.val === '$') return evar('app');
    if(x.val === 'return') return ereturn;
    if(x.val === 'pure') return epure;
    if(x.val === 'varempty') return evarempty;
    if(x.val.startsWith('emb') && x.val.length > 3) return eembed(x.val.slice(3));
    if(x.val.startsWith('cs') && x.val.length > 2) return ecase(x.val.slice(2));
    if(x.val.startsWith('vupd') && x.val.length > 4) return evarupdate(x.val.slice(4));
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
  if(x.type === '{') {
    const v = x.val;
    if(v.length === 0) return eempty;
    if(v.length === 1) throw new SyntaxError(`invalid record`);
    const spl1 = splitOn(v, x => isToken(x, '|'));
    if(spl1.length > 2 || spl1.length === 0) throw new SyntaxError(`invalid use of | in record`);
    const isPlaceholder = spl1.length === 2 && (spl1[1].length === 0 || (spl1[1].length === 1 && isToken(spl1[1][0], '_')));
    const rest = isPlaceholder? evar('_'): spl1.length === 1? eempty: exprs(spl1[1]);
    const contents = spl1[0];
    if(contents.length === 0) throw new SyntaxError(`invalid record`);
    const spl2 = splitOn(contents, x => isToken(x, ','));
    if(spl2.length === 0) throw new SyntaxError(`invalid record`);
    const ret = spl2.reduceRight((e, prop) => {
      if(prop.length === 0) throw new SyntaxError(`invalid record property`);
      const head = prop[0];
      if(head.tag !== 'token') throw new SyntaxError(`invalid record property`);
      if(head.val === '-') {
        if(prop.length !== 2 || prop[1].tag !== 'token') throw new SyntaxError(`invalid record property`);
        return eapp(erestrict(prop[1].val as string), e);
      }
      if(prop.length < 3) throw new SyntaxError(`invalid record property`);
      const operator = prop[1];
      if(operator.tag !== 'token') throw new SyntaxError(`invalid record property`);
      if(operator.val === '=') {
        return eapps(eextend(head.val), exprs(prop.slice(2)), e);
      } else if(operator.val === ':=') {
        return eapps(erecset(head.val), exprs(prop.slice(2)), e);
      } else if(operator.val === '::=') {
        return eapps(erecupdate(head.val), exprs(prop.slice(2)), e);
      } else throw new SyntaxError(`invalid record property operator ${operator.val}`);
    }, rest);
    return isPlaceholder? eabs('_', ret): ret;
  }
  if(x.type !== '(') throw new SyntaxError(`invalid bracket ${x.type}`);
  return exprs(x.val);
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
      else if(c.tag === 'paren' && c.type !== '(') throw new SyntaxError(`unexpected bracket in forall ${c.type}`);
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
    if(args.length === 0) throw new SyntaxError('forall without args');
    if(rest.length === 0) throw new SyntaxError(`missing body in forall`);
    return tforalls(args.map(x => typeof x === 'string'? [x, ktype] as [string, Kind]: x), types(rest));
  }
  if(containsToken(x, '->')) return tfuns.apply(null, splitOn(x, x => isToken(x, '->')).map(types));
  return tapps.apply(null, x.map(type));
}

function type(x: Ret): Type {
  if(x.tag === 'token') return /[a-z]/.test(x.val[0])? tvar(x.val): tcon(x.val);
  if(x.type === '{') {
    const v = x.val;
    if(v.length === 0) return tempty;
    if(v.length === 1) throw new SyntaxError(`invalid row type`);
    const spl1 = splitOn(v, x => isToken(x, '|'));
    if(spl1.length > 2 || spl1.length === 0) throw new SyntaxError(`invalid use of | in row type`);
    const rest = spl1.length === 1? undefined: types(spl1[1]);
    const contents = spl1[0];
    if(contents.length === 0) throw new SyntaxError(`invalid row type`);
    const spl2 = splitOn(contents, x => isToken(x, ','));
    if(spl2.length === 0) throw new SyntaxError(`invalid row type`);
    const row: [string, Type][] = spl2.map(prop => {
      const splprop = splitOn(prop, x => isToken(x, ':'));
      if(splprop.length !== 2) throw new SyntaxError(`invalid row type property`);
      const name = splprop[0];
      if(name.length !== 1) throw new SyntaxError(`invalid row type property name`);
      const nameIn = name[0];
      if(nameIn.tag !== 'token') throw new SyntaxError(`invalid row type property name`);
      return [nameIn.val, types(splprop[1])] as [string, Type];
    });
    return trow(row, rest);
  }
  if(x.type !== '(') throw new SyntaxError(`invalid bracket in type ${x.type}`);
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
    else if(c.tag === 'paren' && c.type !== '(') throw new SyntaxError(`unexpected bracket in data ${c.type}`);
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
    const x = tokenize(s);
    if(x.length < 3 || x[0].tag !== 'token') throw new SyntaxError('invalid def'); 
    const name = x[0].val as string;
    if(!/[a-z][A-Z0-9a-z]*/.test(name)) throw new SyntaxError(`invalid name: ${name}`);
    const args: (string | [string, Type])[] = [];
    let found = -1;
    for(let i = 1; i < x.length; i++) {
      const c = x[i];
      if(isToken(c, '=')) {
        found = i;
        break;
      } else if(c.tag === 'token') args.push(c.val);
      else if(c.tag === 'paren' && c.type !== '(') throw new SyntaxError(`unexpected bracket in def ${c.type}`);
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
    if(found < 0) throw new SyntaxError(`missing = after def`);
    const rest = x.slice(found + 1);
    if(rest.length === 0) throw new SyntaxError(`missing body in function`);
    const body = exprs(rest);
    return new DValue(name, args.length === 0? body: eabss(args, body));
  }
}

export function parseProgram(s: string): Definition[] {
  return s.split(';;').filter(x => x.trim().length > 0).map(x => parseDefinition(x.trim()));
}
