import { Type, isTFun, containsTCon, showTy } from './types';
import { terr } from './util';
import { log } from './config';

export const positivityCheck = (c: string, t: Type, b: boolean = false): void => {
  log(() => `positivityCheck ${c} ${showTy(t)} ${b}`);
  if (isTFun(t)) {
    positivityCheck(c, t.left.right, !b);
    positivityCheck(c, t.right, b);
    return;
  }
  if (t.tag === 'TForall') {
    positivityCheck(c, t.type, b);
    return;
  }
  if (t.tag === 'TApp') {
    positivityCheck(c, t.left, b);
    if (containsTCon(c, t.right))
      return terr(`positivity check failed: ${c}`);
    return;
  }
  if (t.tag === 'TCon') {
    if (t.name !== c) return;
    if (b) return terr(`positivity check failed: ${c}`);
    return;
  }
};
