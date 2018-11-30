import initialContext from "./initial";
import { cvar, ctvar, ceff, cop } from "./elems";
import { tvar, tfun, teffs, tapps, teffsFrom, tforalls, tfuns, isTEffsEmpty } from "./types";
import { name } from "./NameRep";
import { kType, kEff, kfuns, kEffs } from "./kinds";
import parse from "./parser";
import { infer } from "./inference";
import compileToJS from "./javascriptBackend";
import { Right } from "./backup/generic/Either";

export const _context = initialContext.add(
  ctvar(name('Str'), kType),
  cvar(name('show'), tforalls([[name('t'), kType]], tfuns(tvar(name('t')), tvar(name('Str'))))),

  ctvar(name('Void'), kType),
  cvar(name('caseVoid'), tforalls([[name('t'), kType]], tfuns(tvar(name('Void')), tvar(name('t'))))),

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

  cvar(name('fix'), tforalls([[name('t'), kType], [name('e'), kEffs]],
    tfun(tfun(tvar(name('t')), tvar(name('t')), tvar(name('e'))), tvar(name('t')), tvar(name('e'))))),

  ceff(name('Flip')),
  cop(name('flip'), name('Flip'), tvar(name('Unit')), tvar(name('Bool'))),
  ctvar(name('Flip'), kEff),
  cvar(name('flip'), tfun(tvar(name('Unit')), tvar(name('Bool')), teffs(tvar(name('Flip'))))),

  ceff(name('Fail')),
  cop(name('fail'), name('Fail'), tvar(name('Unit')), tvar(name('Void'))),
  ctvar(name('Fail'), kEff),
  cvar(name('fail'), tfun(tvar(name('Unit')), tvar(name('Void')), teffs(tvar(name('Fail'))))),
  
  ceff(name('StateBool')),
  cop(name('get'), name('StateBool'), tvar(name('Unit')), tvar(name('Bool'))),
  cop(name('put'), name('StateBool'), tvar(name('Bool')), tvar(name('Unit'))),
  ctvar(name('StateBool'), kEff),
  cvar(name('get'), tfun(tvar(name('Unit')), tvar(name('Bool')), teffs(tvar(name('StateBool'))))),
  cvar(name('put'), tfun(tvar(name('Bool')), tvar(name('Unit')), teffs(tvar(name('StateBool'))))),
);

function _show(x: any): string {
  if (typeof x === 'string') return JSON.stringify(x);
  if (typeof x === 'function') return '[Function]';
  if (typeof x._tag === 'string')
    return typeof x.val === 'undefined' ? x._tag :
    Array.isArray(x.val) ? `(${x._tag} ${x.val.map(_show).join(' ')})` :
    `(${x._tag} ${_show(x.val)})`;
  if (x._cont) return `${x.op}(${_show(x.val)})`;
  return '' + x;
}

let _ctx = _context;
export default function _run(i: string, cb: (output: string, err?: boolean) => void): void {
  try {
    console.log(i);
    const p = parse(i);
    console.log(''+p);
    const result = infer(_ctx, p).throw();
    console.log(`${result.type}!${result.eff}`);
    const c = compileToJS(p);
    console.log(c);
    const res = eval(c);
    cb(`${_show(res)} : ${result.type.pretty()}${isTEffsEmpty(result.eff) ? '' : `!${(result.eff as any).pretty()}`}`);
  } catch(e) {
    console.log(e);
    cb(''+e, true);
  }
}
