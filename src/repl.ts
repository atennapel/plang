import initialContext from "./il/initial";
import { cvar, ctvar } from "./il/elems";
import { tvar, isTEffsEmpty, tfun, teffs, tapps, teffsFrom, tfunps, tforallps, tcomp } from "./il/types";
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
  cvar(name('show'), tforallps([[name('t'), kType]], tfunps(tvar(name('t')), tvar(name('Str'))))),

  ctvar(name('Void'), kType),
  cvar(name('caseVoid'), tforallps([[name('t'), kType]], tfunps(tvar(name('Void')), tvar(name('t'))))),

  ctvar(name('Unit'), kType),
  cvar(name('Unit'), tvar(name('Unit'))),

  ctvar(name('Bool'), kType),
  cvar(name('True'), tvar(name('Bool'))),
  cvar(name('False'), tvar(name('Bool'))),
  cvar(name('caseBool'), tforallps([[name('t'), kType]], tfunps(tvar(name('t')), tvar(name('t')), tvar(name('Bool')), tvar(name('t'))))),

  ctvar(name('Nat'), kType),
  cvar(name('Z'), tvar(name('Nat'))),
  cvar(name('S'), tfunps(tvar(name('Nat')), tvar(name('Nat')))),
  cvar(name('caseNat'), tforallps([[name('t'), kType]], tfunps(tvar(name('t')), tfunps(tvar(name('Nat')), tvar(name('t'))), tvar(name('Nat')), tvar(name('t'))))),

  ctvar(name('Maybe'), kfuns(kType, kType)),
  cvar(name('Nothing'), tforallps([[name('t'), kType]], tapps(tvar(name('Maybe')), tvar(name('t'))))),
  cvar(name('Just'), tforallps([[name('t'), kType]], tfunps(tvar(name('t')), tapps(tvar(name('Maybe')), tvar(name('t')))))),
  cvar(name('caseMaybe'), tforallps([[name('t'), kType], [name('r'), kType]],
    tfunps(tvar(name('r')), tfunps(tvar(name('t')), tvar(name('r'))), tapps(tvar(name('Maybe')), tvar(name('t'))), tvar(name('r'))))),
  
  ctvar(name('List'), kfuns(kType, kType)),
  cvar(name('Nil'), tforallps([[name('t'), kType]], tapps(tvar(name('List')), tvar(name('t'))))),
  cvar(name('Cons'), tforallps([[name('t'), kType]], tfunps(tvar(name('t')), tapps(tvar(name('List')), tvar(name('t'))), tapps(tvar(name('List')), tvar(name('t')))))),
  cvar(name('caseList'), tforallps([[name('t'), kType], [name('r'), kType]],
    tfunps(tvar(name('r')), tfunps(tvar(name('t')), tapps(tvar(name('List')), tvar(name('t'))), tvar(name('r'))), tapps(tvar(name('List')), tvar(name('t'))), tvar(name('r'))))),

  cvar(name('fix'), tforallps([[name('t'), kType]], tfunps(tfunps(tvar(name('t')), tvar(name('t'))), tvar(name('t'))))),

  ctvar(name('Flip'), kEff),
  cvar(name('flip'), tfun(tvar(name('Unit')), tcomp(tvar(name('Bool')), teffs(tvar(name('Flip')))))),
  cvar(name('runFlip'), tforallps([[name('e'), kEffs], [name('t'), kType]],
    tfun(
      tfun(tvar(name('Unit')), tcomp(tvar(name('t')), teffsFrom([tvar(name('Flip'))], tvar(name('e'))))),
      tcomp(tvar(name('t')), tvar(name('e'))),
    ))),

  ctvar(name('Fail'), kEff),
  cvar(name('fail'), tforallps([[name('t'), kType]], tfun(tvar(name('Unit')), tcomp(tvar(name('t')), teffs(tvar(name('Fail'))))))),
  cvar(name('runFail'), tforallps([[name('e'), kEffs], [name('t'), kType]],
    tfun(
      tfun(tvar(name('Unit')), tcomp(tvar(name('t')), teffsFrom([tvar(name('Fail'))], tvar(name('e'))))),
      tcomp(tapps(tvar(name('Maybe')), tvar(name('t'))), tvar(name('e'))),
    ))),

  ctvar(name('State'), kEff),
  cvar(name('get'), tforallps([[name('t'), kType]], tfun(tvar(name('Unit')), tcomp(tvar(name('Nat')), teffs(tvar(name('State'))))))),
  cvar(name('put'), tforallps([[name('t'), kType]], tfun(tvar(name('Nat')), tcomp(tvar(name('Unit')), teffs(tvar(name('State'))))))),
  cvar(name('runState'), tforallps([[name('e'), kEffs], [name('t'), kType]],
    tfunps(
      tvar(name('Nat')),
      tfun(
        tfun(tvar(name('Unit')), tcomp(tvar(name('t')), teffsFrom([tvar(name('State'))], tvar(name('e'))))),
        tcomp(tvar(name('t')), tvar(name('e'))),
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
    console.log(`${result}`);
    if (cex) {
      const c = compileToJS(cex);
      console.log(c);
      const res = eval(c);
      cb(`${_show(res)} : ${result.pretty()}`);
    }
  } catch(e) {
    console.log(e);
    cb(''+e, true);
  }
}
