type Expr = EVar | EAbs | EApp;

interface EVar { tag: 'EVar', val: { name: string } }
const evar = (name: string): EVar => ({
  tag: 'EVar',
  val: { name },
});

interface EAbs { tag: 'EAbs', val: { arg: string, expr: Expr } }
const eabs = (arg: string, expr: Expr): EAbs => ({
  tag: 'EAbs',
  val: { arg, expr },
});
const eabss = (args: string[], expr: Expr) =>
  args.reduceRight((e, arg) => eabs(arg, e), expr);

interface EApp { tag: 'EApp', val: { left: Expr, right: Expr } }
const eapp = (left: Expr, right: Expr): EApp => ({
  tag: 'EApp',
  val: { left, right },
});
const eapps = (...es: Expr[]) => es.reduce(eapp);

const exprStr = (expr: Expr): string => {
  switch(expr.tag) {
    case 'EVar': return expr.val.name;
    case 'EAbs':
      return `(\\${expr.val.arg} . ${exprStr(expr.val.expr)})`;
    case 'EApp':
      return `(${exprStr(expr.val.left)} ${exprStr(expr.val.right)})`;
  }
};

const add = <T>(a: T[], v: T) => {
  if(a.indexOf(v) < 0) return a.concat([v]);
  return a;
};
const remove = <T>(a: T[], v: T) => {
  const r: T[] = [];
  for(let i = 0, l = a.length; i < l; i++)
    if(a[i] !== v) r.push(a[i]);
  return r;
};
const union = <T>(a: T[], b: T[]): T[] => {
  const r: T[] = a.slice();
  for(let i = 0, l = b.length; i < l; i++)
    if(r.indexOf(b[i]) < 0) r.push(b[i]);
  return r;
};

const free = (expr: Expr, a: string[] = []): string[] => {
  switch(expr.tag) {
    case 'EVar': return add(a, expr.val.name);
    case 'EAbs':
      return remove(free(expr.val.expr), expr.val.arg);
    case 'EApp':
      return union(free(expr.val.left), free(expr.val.right));
  }
};
const subst = (expr: Expr, x: string, e: Expr): Expr => {
  switch(expr.tag) {
    case 'EVar': return expr.val.name === x? e: expr;
    case 'EAbs':
      if(free(expr).indexOf(x) < 0) return expr;
      if(x !== expr.val.arg && free(e).indexOf(expr.val.arg) < 0)
        return eabs(expr.val.arg, subst(expr.val.expr, x, e));
      return expr;
    case 'EApp':
      return eapp(subst(expr.val.left, x, e), subst(expr.val.right, x, e));
  }
}
