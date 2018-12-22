import { Env } from "./env";
import { Var, showExpr } from "./exprs";
import { TVar, TRowEmpty, TVariant, tapp, tfuns, TRecord, TConst, showType, prettyType, TStr } from "./types";
import { KRow, kfun, KType } from "./kinds";
import { parse } from "./parser";
import { inferTop } from "./inference";
import { compileToJS } from "./compiler";

const tv = TVar;
const ta = tv(0);
const tb = tv(1);
const tr = tv(1, KRow);

const _env: Env = {
  fix: tfuns(tfuns(ta, ta), ta),
  empty: tapp(TRecord, TRowEmpty),
  end: tfuns(tapp(TVariant, TRowEmpty), ta),
  show: tfuns(ta, TStr),
};

function _show(x: any): string {
  if (typeof x === 'string') return JSON.stringify(x);
  if (typeof x === 'function') return '[Function]';
  if (typeof x._tag === 'string')
    return typeof x.val === 'undefined' ? x._tag :
      Array.isArray(x.val) ? `(${x._tag} ${x.val.map(_show).join(' ')})` :
      `(${x._tag} ${_show(x.val)})`;
  if (typeof x === 'object' && x._rec) {
    const r = [];
    for (let k in x) if (k[0] !== '_') r.push(`${k}: ${_show(x[k])}`);
    return `{${r.join(', ')}}`;
  }
  return '' + x;
}

export default function _run(i: string, cb: (output: string, err?: boolean) => void): void {
  try {
    console.log(i);
    const p = parse(i);
    console.log(showExpr(p));
    const result = inferTop(_env, p);
    console.log(`${showType(result)}`);
    const c = compileToJS(p);
    console.log(c);
    const res = eval(c);
    cb(`${_show(res)} : ${prettyType(result)}`);
  } catch(e) {
    console.log(e);
    cb(''+e, true);
  }
}
