import { Type, flattenTFun, isTFun } from './types';
import { terr } from './util';

export const positivityCheckArg = (c: string, t: Type, b = true): void => {
  if (isTFun(t)) {
    positivityCheckArg(c, t.left.right, !b);
    positivityCheckArg(c, t.right, b);
    return;
  }
  if (t.tag === 'TApp') {
    positivityCheckArg(c, t.left, b);
    positivityCheckArg(c, t.right, b);
    return;
  }
  if (t.tag === 'TCon' && t.name === c) {
    if (!b) return terr(`positivity check failed: ${c}`);
    return;
  }
};

export const positivityCheck = (c: string, t: Type) => {
  const ty = t.tag === 'TForall' ? t.type : t;
  const args = flattenTFun(ty).slice(0, -1);
  for (let i = 0; i < args.length; i++)
    positivityCheckArg(c, args[i]);
};
