import { showType } from './types';
import { showTerm } from './terms';
import { infer, inferDefs } from './inference';
import { compile, compileDefs } from './compiler';
import { parse, parseDefs, parseType } from './parser';
import { context } from './global';
import { showDef } from './definitions';

/*
const scr = '\\x -> x';
try {
  console.log(scr);
  const term = parse(scr);
  console.log(showTerm(term));
  const ty = infer(term);
  console.log(showType(ty));
  console.log(`${context}`);
  const co = compile(term);
  console.log(co);
  const ev = eval(co);
  console.log(ev);
} catch (err) {
  console.log(err);
}
*/
const s = '->';
const t = parseType(s);
console.log(showType(t));
