import { Kind, KMeta } from './kinds';
import { Type, openTForall, TVar } from './types';
import { Elem, CTVar, CKMeta, showElem } from './elems';
import { Context } from './context';
import { context, restoreContext, storeContext, namestore } from './global';
import { showName } from './names';
import { infererr } from './error';

export const wfKind = (kind: Kind): void => {
  switch (kind.tag) {
    case 'KVar': {
      if (context.contains('CKVar', kind.name)) return;
      return infererr(`undefined kind ${showName(kind.name)}`);
    }
    case 'KMeta': {
      if (context.contains('CKMeta', kind.name)) return;
      return infererr(`undefined kind ?${showName(kind.name)}`);
    }
    case 'KFun': {
      wfKind(kind.left);
      return wfKind(kind.right);
    }
  }
};

export const wfType = (type: Type): void => {
  switch (type.tag) {
    case 'TVar': {
      if (context.contains('CTVar', type.name)) return;
      return infererr(`undefined type ${showName(type.name)}`);
    }
    case 'TMeta': {
      if (context.contains('CTMeta', type.name)) return;
      return infererr(`undefined type ?${showName(type.name)}`);
    }
    case 'TApp': {
      wfType(type.left);
      return wfType(type.right);
    }
    case 'TForall': {
      if (type.kind) wfKind(type.kind);
      const t = namestore.fresh(type.name);
      if (type.kind) {
        context.enter(t, CTVar(t, type.kind));
      } else {
        const k = namestore.fresh(type.name);
        context.enter(t, CKMeta(k), CTVar(t, KMeta(k)));
      }
      wfType(openTForall(type, TVar(t)));
      context.leave(t);
      return;
    }
  }
};

export const wfElem = (elem: Elem): void => {
  // console.log(`wfElem ${showElem(elem)} in ${context}`);
  switch (elem.tag) {
    case 'CKVar': {
      if (!context.contains('CKVar', elem.name)) return;
      return infererr(`duplicate kind ${showName(elem.name)}`);
    }
    case 'CKMeta': {
      if (context.contains('CKMeta', elem.name))
        return infererr(`duplicate kind ?${showName(elem.name)}`);
      if (elem.kind) wfKind(elem.kind);
      return;
    }
    case 'CTVar': {
      if (context.contains('CTVar', elem.name))
        return infererr(`duplicate type ${showName(elem.name)}`);
      return wfKind(elem.kind);
    }
    case 'CTMeta': {
      if (context.contains('CTMeta', elem.name))
        return infererr(`duplicate type ?${showName(elem.name)}`);
      wfKind(elem.kind);
      if (elem.type) wfType(elem.type);
      return;
    }
    case 'CVar': {
      if (context.contains('CVar', elem.name))
        return infererr(`duplicate var ${showName(elem.name)}`);
      return wfType(elem.type);
    }
    case 'CMarker': {
      if (!context.contains('CMarker', elem.name)) return;
      return infererr(`duplicate marker ${showName(elem.name)}`);
    }
  }
};

export const wfContext = (): void => {
  storeContext();
  let elem: Elem | null = context.pop();
  while (elem) {
    wfElem(elem);
    elem = context.pop();
  }
  restoreContext();
};

