import { Type, isTVar, isTMeta, isTFun, isTForall, openTForall, TVar } from './types';
import { impossible } from './errors';
import { context, withElems, findElem, findElemNot, setContext } from './context';
import { freshName } from './names';
import { CTVar, matchCTVar, matchCTMeta, Elem, isCTVar, isCTMeta, isCVar, isCMarker, matchCVar, matchCMarker } from './elems';

export const wfType = (type: Type): void => {
  if (isTVar(type)) {
    findElem(matchCTVar(type.name), `undefined TVar ${type.name}`);
    return;
  }
  if (isTMeta(type)) {
    findElem(matchCTMeta(type.name), `undefined TMeta ?${type.name}`);
    return;
  }
  if (isTFun(type)) {
    wfType(type.left);
    wfType(type.right);
    return;
  }
  if (isTForall(type)) {
    const x = freshName(type.name);
    withElems([CTVar(x)], () => wfType(openTForall(type, TVar(x))));
    return;
  }
  return impossible('wfType');
};

export const wfElem = (elem: Elem): void => {
  if (isCTVar(elem)) return findElemNot(matchCTVar(elem.name), `duplicate CTVar ${elem.name}`);
  if (isCTMeta(elem)) return findElemNot(matchCTMeta(elem.name), `duplicate CTMeta ?${elem.name}`);
  if (isCVar(elem)) return findElemNot(matchCVar(elem.name), `duplicate CVar ${elem.name}`);
  if (isCMarker(elem)) {
    const x = elem.name;
    return findElemNot(e => matchCMarker(x)(e) || matchCTMeta(x)(e), `duplicate CMarker |>${x}`);
  }
  return impossible('wfElem');
};

export const wfContext = (): void => {
  const old = context.slice(0);
  while(context.length > 0) {
    const elem = context.pop() as Elem;
    wfElem(elem);
  }
  setContext(old);
};
