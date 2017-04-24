type ExprTag =
  'EVar' |
  'EApp' |
  'ELam' |
  'ELet';

export interface Expr { tag : ExprTag };

interface EVar extends Expr { name : string };
export const isEVar = (e: Expr): e is EVar => e.tag === 'EVar';
export const evar = (name: string): EVar => ({
  tag: 'EVar',
  name,
});

interface EApp extends Expr { left: Expr, right: Expr };
export const isEApp = (e: Expr): e is EApp => e.tag === 'EApp';
export const eapp = (left: Expr, right: Expr): EApp => ({
  tag: 'EApp',
  left,
  right,
});
export const eapp_ = (a: Expr, b: Expr, ...rest: Expr[]): EApp => {
  const l = rest.length;
  if(l === 0) return eapp(a, b);
  if(l === 1) return eapp(eapp(a, b), rest[0]);
  let c = eapp(a, b);
  for(let i = 0; i < l; i++) c = eapp(c, rest[i]);
  return c;
};

interface ELam extends Expr { arg: string, body: Expr };
export const isELam = (e: Expr): e is ELam => e.tag === 'ELam';
export const elam = (arg: string, body: Expr): ELam => ({
  tag: 'ELam',
  arg,
  body,
});
export const elam_ = (args: string[], body: Expr): Expr => {
  let c = body;
  const l = args.length;
  for(let i = l - 1; i >= 0; i--) c = elam(args[i], c);
  return c;
};

interface ELet extends Expr { arg: string, val: Expr, body: Expr };
export const isELet = (e: Expr): e is ELet => e.tag === 'ELet';
export const elet = (arg: string, val: Expr, body: Expr): ELet => ({
  tag: 'ELet',
  arg,
  val,
  body,
});

export const exprStr = (e: Expr): string => {
  if(isEVar(e)) return e.name;
  if(isEApp(e)) return `(${exprStr(e.left)} ${exprStr(e.right)})`;
  if(isELam(e)) return `(\\${e.arg} -> ${exprStr(e.body)})`;
  if(isELet(e))
    return `(let ${e.arg} = ${exprStr(e.val)} in ${exprStr(e.body)})`;
  throw new Error('impossible');
};
