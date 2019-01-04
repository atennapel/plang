import { Name, eqName, showName } from './names';
import { Type, showType } from './types';
import { log } from './logging';

const simplifyConstraints = (cs: Name[]): Name[] => {
  const r: Name[] = [];
  for (let i = 0, l = cs.length; i < l; i++) {
    const c = cs[i];
    let found = false;
    for (let j = 0, k = r.length; j < k; k++) {
      if (eqName(c, r[k])) {
        found = true;
        break;
      }
    }
    if (!found) r.push(c);
  }
  return r;
};

const solveConstraint = (c: Name, type: Type, unif: boolean = false): boolean => {
  log(`solveConstraint: ${showName(c)} ${showType(type)} | ${unif}`);
  return false;
};

export const solveConstraints = (cs: Name[], type: Type, unif: boolean = false): Name[] => {
  const a = simplifyConstraints(cs);
  const ret: Name[] = [];
  for (let i = 0, l = a.length; i < l; i++) {
    const c = a[i];
    const r = solveConstraint(c, type, unif);
    if (!r) ret.push(c);
  }
  return ret;
};
