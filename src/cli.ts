import * as fs from 'fs';

import { compileProgram } from './compilerJS';
import { isErr, isOk } from './Result';
import { inferProgram, initialContext } from './typechecker';
import { parseProgram } from './parser'
import { ppType, ppContext } from './prettyprinter';

const lib = fs.readFileSync('lib.compile.js', {encoding: 'utf8'});
const inp = fs.readFileSync(process.argv[2], {encoding: 'utf8'});
const ds = parseProgram(inp);
const t = inferProgram(initialContext, ds);
if(isErr(t)) throw t.err;
else if(isOk(t)) {
  console.log(ppContext(t.val));
  console.log(ppType(t.val.apply(t.val.findVar('main') as any)));
}
const out = compileProgram(ds, true, lib);
fs.writeFileSync(process.argv[3] || `${process.argv[2]}.js`, out, {encoding:'utf8'});

process.exit();
