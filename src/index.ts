import { showType } from './types';
import { showTerm } from './terms';
import { infer } from './inference';
import { compile } from './compiler';
import { parse } from './parser';

const scr = '\\x -> x';
try {
  console.log(scr);
  const term = parse(scr);
  console.log(showTerm(term));
  const ty = infer(term);
  console.log(showType(ty));
  const co = compile(term);
  console.log(co);
  const ev = eval(co);
  console.log(ev);
} catch (err) {
  console.log(err);
}
