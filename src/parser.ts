import { Expr, eapps, evar, EVar, eabss } from './exprs';

function matchingBracket(c: string) {
  if(c === '(') return ')';
  if(c === ')') return '(';
  return '';
}

export function parse(s: string): Expr {
  const START = 0, NAME = 1;
  let state = START;
  let r = [], p = [], b = [];
  let t = '';
  for(let i = 0; i <= s.length; i++) {
    const c = s[i] || ' ';
    if(state === START) {
      if(/[a-z]/i.test(c)) t += c, state = NAME;
      else if(c === '-' && s[i+1] === '>') r.push(evar('->')), i++;
      else if(c === '(') b.push(c), p.push(r), r = [];
      else if(c === ')') {
        if(b.length === 0) throw new SyntaxError(`unmatched bracket: ${c}`);
        const br = b.pop() as string;
        if(matchingBracket(br) !== c) throw new SyntaxError(`unmatched bracket: ${br} and ${c}`);
        const a: any[] = p.pop() as any[];
        a.push(makeApp(r));
        r = a;
      }
    } else if(state === NAME) {
      if(!/[a-z0-9\']/i.test(c)) r.push(evar(t)), t = '', i--, state = START;
      else t += c;
    }
  }
  if(state !== START) throw new SyntaxError(`invalid parsing state: ${state}`);
  return makeApp(r);
}

function byName(x: Expr, n: string): x is EVar {
  return x instanceof EVar && x.name === n;
}

function makeApp(r: Expr[]): Expr {
  console.log(r.join(' '));
  if(r.length === 0) return evar('unit');
  if(r.length === 1) return r[0];
  if(byName(r[0], 'fn')) {
    const args = [];
    const found = -1;
    for(let i = 1; i < r.length; i++) {
      const c = r[i];
      if(byName(c, '->')) {
        const found = i;
        break;
      } else if(c instanceof EVar) {
        args.push(c.name);
      } else throw new SyntaxError(`invalid fn arg: ${c}`);
    }
    if(found < 0) throw new SyntaxError(`missing -> after fn`);
    const rest = r.slice(found + 1);
    if(rest.length === 0) throw new SyntaxError(`fn without body`);
    return eabss(args, makeApp(rest));
  }
  return eapps.apply(null, r);
}
