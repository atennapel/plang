import Expr, { vr, apps, appFrom, lt, abs, abss, absty, lts, app } from './exprs';
import { any } from '../utils';
import { tvar } from './types';

function matchingBracket(c: string) {
  if(c === '(') return ')';
  if(c === ')') return '(';
  if(c === '{') return '}';
  if(c === '}') return '{';
  if(c === '[') return ']';
  if(c === ']') return '[';
  return '';
}

type Bracket = '[' | '{' | '(';

type Token
  = { tag: 'name', val: string }
  | { tag: 'list', val: Token[], br: Bracket };

const START = 0;
const NAME = 1;

function tokenize(s: string): Token[] {
  let state = START;
  let t = '';
  let r: Token[] = [], p: Token[][] = [], b: Bracket[] = [];
  for (let i = 0; i <= s.length; i++) {
    const c = s[i] || ' ';
    if (state === START) {
      if (/[a-z\:\_]/i.test(c)) t += c, state = NAME;
      else if(c === '(' || c === '{' || c === '[') b.push(c), p.push(r), r = [];
      else if(c === ')' || c === '}' || c === ']') {
        if(b.length === 0) throw new SyntaxError(`unmatched bracket: ${c}`);
        const br = b.pop() as Bracket;
        if(matchingBracket(br) !== c) throw new SyntaxError(`unmatched bracket: ${br} and ${c}`);
        const a: Token[] = p.pop() as Token[];
        a.push({ tag: 'list', val: r, br });
        r = a;
      } else if(/\s+/.test(c)) continue;
      else throw new SyntaxError(`invalid char: ${c}`);
    } else if (state === NAME) {
      if(!/[a-z0-9\_\!]/i.test(c)) r.push({ tag: 'name', val: t }), t = '', i--, state = START;
      else t += c;
    }
  }
  if(state !== START) throw new SyntaxError(`invalid parsing end state: ${state}`);
  return r;
}

function exprs(r: Token[], br: Bracket = '['): Expr {
  switch(br) {
    case '(': return r.length === 0 ? vr('Unit') : r.length === 1 ? expr(r[0]) : appFrom(r.map(expr));
    case '[':
      if (r.length === 0) return vr('Nil');
      if (r.length === 1) return expr(r[0]);
      let n: string|null = null;
      let res: [string, Expr][] = [];
      for (let i = 0; i < r.length - 1; i++) {
        const c = r[i];
        if (n === null) {
          if (c.tag === 'name' && c.val[0] === ':') {
            n = c.val.slice(1);
          } else {
            res.push(['_', expr(c)]);
          }
        } else {
          if (c.tag === 'name' && c.val[0] === ':') {
            res.push([n, vr('Unit')] as [string, Expr]);
            n = null;
            i--;
          } else {
            res.push([n, expr(c)]);
            n = null;
          }
        }
      }
      return lts(res, expr(r[r.length - 1]));
    case '{':
      if (r.length === 0) return abs('x', vr('x'));
      if (r.length === 1) return abs('_', expr(r[0]));
      const args = r[0];
      if (args.tag !== 'list' || args.br !== '[') return abs('_', exprs(r, '('));
      if (any(args.val, a => a.tag !== 'name')) throw new SyntaxError(`invalid args: ${args.val.join(' ')}`);
      return abss(args.val.map(a => a.tag === 'name' ? a.val : null).filter(Boolean) as string[], exprs(r.slice(1), '('));
  }
}

function expr(r: Token): Expr {
  switch(r.tag) {
    case 'name': return r.val[r.val.length - 1] === '!' ? app(vr(r.val.slice(0, -1)), vr('Unit')) : vr(r.val);
    case 'list': return exprs(r.val, r.br);
  }
}

export default function parse(s: string): Expr {
  return exprs(tokenize(s), '(');
}
