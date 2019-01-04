import { Type, isTVar, isTMeta, isTFun, isTForall, openTForall, TVar, showType, isTApp } from './types';
import { impossible } from './errors';
import { context, withElems, findElem, findElemNot, setContext } from './context';
import { freshName, showName } from './names';
import { CTVar, matchCTVar, matchCTMeta, Elem, isCTVar, isCTMeta, isCVar, isCMarker, matchCVar, matchCMarker, matchCKMeta, matchCKVar, isCKVar, isCKMeta, isCClass, matchCClass } from './elems';
import { isKVar, isKMeta, isKFun, showKind, Kind } from './kinds';

export const wfKind = (kind: Kind): void => {
  if (isKVar(kind)){
    findElem(matchCKVar(kind.name), `undefined KVar ${showKind(kind)}`);
    return;
  }
  if (isKMeta(kind)) {
    findElem(matchCKMeta(kind.name), `undefined KMeta ${showKind(kind)}`);
    return;
  }
  if (isKFun(kind)){
    wfKind(kind.left);
    wfKind(kind.right);
    return;
  }
  return impossible('wfKind');
};

export const wfType = (type: Type): void => {
  if (isTVar(type)) {
    findElem(matchCTVar(type.name), `undefined TVar ${showType(type)}`);
    return;
  }
  if (isTMeta(type)) {
    findElem(matchCTMeta(type.name), `undefined TMeta ${showType(type)}`);
    return;
  }
  if (isTFun(type)) {
    wfType(type.left);
    wfType(type.right);
    return;
  }
  if (isTApp(type)) {
    wfType(type.left);
    wfType(type.right);
    return;
  }
  if (isTForall(type)) {
    wfKind(type.kind);
    type.cs.forEach(c => findElem(matchCClass(c), `undefined class ${showName(c)} in ${showType(type)}`));
    const x = freshName(type.name);
    withElems([CTVar(x, type.kind)], () => wfType(openTForall(type, TVar(x))));
    return;
  }
  return impossible('wfType');
};

export const wfElem = (elem: Elem): void => {
  if (isCKVar(elem)) return findElemNot(matchCKVar(elem.name), `duplicate CKVar ${showName(elem.name)}`);
  if (isCKMeta(elem)) return findElemNot(matchCKMeta(elem.name), `duplicate CKMeta ?${showName(elem.name)}`);
  if (isCClass(elem)) return findElemNot(matchCClass(elem.name), `duplicate CClass ${showName(elem.name)}`);
  if (isCTVar(elem)) {
    wfKind(elem.kind);
    return findElemNot(matchCTVar(elem.name), `duplicate CTVar ${showName(elem.name)}`);
  }
  if (isCTMeta(elem)) {
    wfKind(elem.kind);
    return findElemNot(matchCTMeta(elem.name), `duplicate CTMeta ?${showName(elem.name)}`);
  }
  if (isCVar(elem)) return findElemNot(matchCVar(elem.name), `duplicate CVar ${showName(elem.name)}`);
  if (isCMarker(elem)) {
    const x = elem.name;
    return findElemNot(e => matchCMarker(x)(e) || matchCTMeta(x)(e), `duplicate CMarker |>${showName(x)}`);
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
