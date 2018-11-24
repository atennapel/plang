import ILKind, { kvar, kfun } from '../il/kinds';
import ILType, { tvar, tapp, tfun, tforall, teffsempty, teffsextend } from '../il/types';
import ILVal, { vr, abs, absty, absT, anno } from '../il/values';
import ILComp, { ret, appT, lt, app } from '../il/computations';
import Kind, { isKVar, isKFun } from './kinds';
import Type, { isTVar, isTApp, isTFun, isTForall, isTEffsEmpty, isTEffsExtend } from './types';
import Expr, { isVar, isAbs, isAbsT, isAnno, isReturn, isApp, isAppT, isLet } from './exprs';
import TC, { error, freshName, freshNames } from '../il/TC';
import { impossible } from '../backup/utils';
import { name } from '../NameRep';

export const kindToIL = (k: Kind): TC<ILKind> => {
  if (isKVar(k)) return TC.of(kvar(name(k.name)));
  if (isKFun(k)) return kindToIL(k.left).chain2((l, r) => TC.of(kfun(l, r)), kindToIL(k.right));
  return impossible();
};

export const typeToIL = (t: Type): TC<ILType> => {
  if (isTVar(t)) return TC.of(tvar(name(t.name)));
  if (isTApp(t)) return typeToIL(t.left).chain2((l, r) => TC.of(tapp(l, r)), typeToIL(t.right));
  if (isTFun(t)) return typeToIL(t.left).chain3((a, b, c) => TC.of(tfun(a, b, c)), typeToIL(t.eff), typeToIL(t.right));
  if (isTForall(t)) return kindToIL(t.kind).chain(k => typeToIL(t.type).map(ty => tforall(name(t.name), k, ty)));
  if (isTEffsExtend(t)) return typeToIL(t.type).chain2((l, r) => TC.of(teffsextend(l, r)), typeToIL(t.rest));
  if (isTEffsEmpty(t)) return TC.of(teffsempty());
  return impossible();
};

export const exprToCompIL = (e: Expr): TC<ILComp> => {
  if (e.isValue()) return exprToValIL(e).map(v => ret(v));

  if (isReturn(e)) return exprToValIL(e.val).map(v => ret(v));
  if (isApp(e)) {
    if (e.left.isValue() && e.right.isValue())
      return exprToValIL(e.left)
        .chain(a => exprToValIL(e.right)
        .map(b => app(a, b)));
    if (e.left.isValue())
      return freshName(name('_'))
        .chain(x => exprToValIL(e.left)
        .chain(a => exprToCompIL(e.right)
        .map(b => lt(x, b, app(a, vr(x))))));
    if (e.right.isValue())
      return freshName(name('_'))
        .chain(x => exprToCompIL(e.left)
        .chain(a => exprToValIL(e.right)
        .map(b => lt(x, a, app(vr(x), b)))));
    return freshNames([name('_'), name('_')])
      .chain(([x, y]) => exprToCompIL(e.left)
      .chain(a => exprToCompIL(e.right)
      .map(b => lt(x, a, lt(y, b, app(vr(x), vr(y)))))));
  }
  if (isAppT(e)) {
    if (e.left.isValue()) return exprToValIL(e.left).chain(v => typeToIL(e.right).map(t => appT(v, t)));
    return freshName(name('_'))
      .chain(x => exprToCompIL(e.left)
      .chain(c => typeToIL(e.right)
      .map(t => lt(x, c, appT(vr(x), t)))));
  }
  if (isLet(e)) return exprToCompIL(e.expr).chain(a => exprToCompIL(e.body).map(b => lt(name(e.name), a, b)));
  return impossible();
};

export const exprToValIL = (e: Expr): TC<ILVal> => {
  if (!e.isValue()) return error(`cannot convert to value: ${e}`);

  if (isVar(e)) return TC.of(vr(name(e.name)));
  if (isAbs(e))
    return e.type ?
      typeToIL(e.type)
        .chain(type => exprToCompIL(e.body)
        .map(body => absty(name(e.name), type, body))) :
      exprToCompIL(e.body)
        .map(body => abs(name(e.name), body));
  if (isAbsT(e))
    return kindToIL(e.kind).chain(k => exprToCompIL(e.body).map(body => absT(name(e.name), k, body)));
  if (isAnno(e))
    return exprToValIL(e.expr).chain(ex => typeToIL(e.type).map(ty => anno(ex, ty)));

  return impossible();
};
