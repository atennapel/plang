import initialContext from "./il/initial";
import { cvar, ctvar } from "./il/elems";
import { tforalls, tfuns, tvar, isTEffsEmpty, tfun, teffs, tapps, teffsFrom } from "./il/types";
import { name } from "./NameRep";
import { kType, kEff, kfuns, kEffs } from "./il/kinds";
import parse from "./surface/parser";
import { exprToCompIL } from "./surface/surface";
import { log } from "./il/TC";
import { synthgenComp } from "./il/inference";
import NameRepSupply from "./NameSupply";
import Comp from "./il/computations";
import compileToJS from "./il/javascriptBackend";

export const _context = initialContext.add(
  ctvar(name('Str'), kType),
  cvar(name('show'), tforalls([[name('t'), kType]], tfuns(tvar(name('t')), tvar(name('Str'))))),

  ctvar(name('Void'), kType),
  cvar(name('caseVoid'), tforalls([[name('t'), kType]], tfuns(tvar(name('Void')), tvar(name('t'))))),

  ctvar(name('Unit'), kType),
  cvar(name('Unit'), tvar(name('Unit'))),

  ctvar(name('Bool'), kType),
  cvar(name('True'), tvar(name('Bool'))),
  cvar(name('False'), tvar(name('Bool'))),
  cvar(name('caseBool'), tforalls([[name('t'), kType]], tfuns(tvar(name('t')), tvar(name('t')), tvar(name('Bool')), tvar(name('t'))))),

  ctvar(name('Nat'), kType),
  cvar(name('Z'), tvar(name('Nat'))),
  cvar(name('S'), tfuns(tvar(name('Nat')), tvar(name('Nat')))),
  cvar(name('caseNat'), tforalls([[name('t'), kType]], tfuns(tvar(name('t')), tfuns(tvar(name('Nat')), tvar(name('t'))), tvar(name('Nat')), tvar(name('t'))))),

  ctvar(name('Maybe'), kfuns(kType, kType)),
  cvar(name('Nothing'), tforalls([[name('t'), kType]], tapps(tvar(name('Maybe')), tvar(name('t'))))),
  cvar(name('Just'), tforalls([[name('t'), kType]], tfuns(tvar(name('t')), tapps(tvar(name('Maybe')), tvar(name('t')))))),
  cvar(name('caseMaybe'), tforalls([[name('t'), kType], [name('r'), kType]],
    tfuns(tvar(name('r')), tfuns(tvar(name('t')), tvar(name('r'))), tapps(tvar(name('Maybe')), tvar(name('t'))), tvar(name('r'))))),
  
  ctvar(name('List'), kfuns(kType, kType)),
  cvar(name('Nil'), tforalls([[name('t'), kType]], tapps(tvar(name('List')), tvar(name('t'))))),
  cvar(name('Cons'), tforalls([[name('t'), kType]], tfuns(tvar(name('t')), tapps(tvar(name('List')), tvar(name('t'))), tapps(tvar(name('List')), tvar(name('t')))))),
  cvar(name('caseList'), tforalls([[name('t'), kType], [name('r'), kType]],
    tfuns(tvar(name('r')), tfuns(tvar(name('t')), tapps(tvar(name('List')), tvar(name('t'))), tvar(name('r'))), tapps(tvar(name('List')), tvar(name('t'))), tvar(name('r'))))),

  cvar(name('fix'), tforalls([[name('t'), kType]], tfuns(tfuns(tvar(name('t')), tvar(name('t'))), tvar(name('t'))))),

  ctvar(name('Flip'), kEff),
  cvar(name('flip'), tfun(tvar(name('Unit')), teffs(tvar(name('Flip'))), tvar(name('Bool')))),
  cvar(name('runFlip'), tforalls([[name('e'), kEffs], [name('t'), kType]],
    tfun(
      tfun(tvar(name('Unit')), teffsFrom([tvar(name('Flip'))], tvar(name('e'))), tvar(name('t'))),
      tvar(name('e')),
      tvar(name('t')),
    ))),

  ctvar(name('Fail'), kEff),
  cvar(name('fail'), tforalls([[name('t'), kType]], tfun(tvar(name('Unit')), teffs(tvar(name('Fail'))), tvar(name('t'))))),
  cvar(name('runFail'), tforalls([[name('e'), kEffs], [name('t'), kType]],
    tfun(
      tfun(tvar(name('Unit')), teffsFrom([tvar(name('Fail'))], tvar(name('e'))), tvar(name('t'))),
      tvar(name('e')),
      tapps(tvar(name('Maybe')), tvar(name('t'))),
    ))),

  ctvar(name('State'), kEff),
  cvar(name('get'), tforalls([[name('t'), kType]], tfun(tvar(name('Unit')), teffs(tvar(name('State'))), tvar(name('Nat'))))),
  cvar(name('put'), tforalls([[name('t'), kType]], tfun(tvar(name('Nat')), teffs(tvar(name('State'))), tvar(name('Unit'))))),
  cvar(name('runState'), tforalls([[name('e'), kEffs], [name('t'), kType]],
    tfuns(
      tvar(name('Nat')),
      tfun(
        tfun(tvar(name('Unit')), teffsFrom([tvar(name('State'))], tvar(name('e'))), tvar(name('t'))),
        tvar(name('e')),
        tvar(name('t')),
      )))),
);

function _show(x: any): string {
  if (typeof x === 'string') return JSON.stringify(x);
  if (typeof x === 'function') return '[Function]';
  if (typeof x._tag === 'string')
    return typeof x.val === 'undefined' ? x._tag :
    Array.isArray(x.val) ? `(${x._tag} ${x.val.map(_show).join(' ')})` :
    `(${x._tag} ${_show(x.val)})`;
  if (x._cont) return `(${x.op}(${_show(x.val)}))`;
  return '' + x;
}

let _ctx = _context;
export default function _run(i: string, cb: (output: string, err?: boolean) => void): void {
  try {
    console.log(i);
    const p = parse(i);
    console.log(''+p);
    let cex: Comp | null = null;
    const prog = exprToCompIL(p)
      .chain(ex => {
        cex = ex;
        return log(`${ex}`).then(synthgenComp(ex))
      });
    const result = prog.run(_ctx, new NameRepSupply(0)).val.throw();
    console.log(`${result.type}!${result.eff}`);
    if (cex) {
      const c = compileToJS(cex);
      console.log(c);
      const res = eval(c);
      cb(`${_show(res)} : ${result.type}${isTEffsEmpty(result.eff) ? '' : `!${result.eff}`}`);
    }
  } catch(e) {
    console.log(e);
    cb(''+e, true);
  }
}
