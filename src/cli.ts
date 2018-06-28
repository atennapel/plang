import * as fs from 'fs';

import { compileProgram } from './compilerJS';
import { inferProgram, initialContext } from './typechecker';
import { parseProgram } from './parser'
import { ppType, ppContext } from './prettyprinter';

const lib = fs.readFileSync('lib.js', {encoding: 'utf8'});
const inp = fs.readFileSync(process.argv[2], {encoding: 'utf8'});
const ds = parseProgram(inp);
const t = inferProgram(initialContext, ds);
console.log(ppContext(t.ctx));
console.log(ppType(t.ctx.apply(t.ctx.findVar('main') as any)));
const out = compileProgram(t.defs, true, lib);
fs.writeFileSync(process.argv[3] || `${process.argv[2]}.js`, out, {encoding:'utf8'});

process.exit();
