import { Term, Pat, LitChar } from './terms';
import { Name, impossible } from './util';
import { Def } from './definitions';

export const compileName = (x: Name) => {
  return keywords.indexOf(x) >= 0 ? `${x}_` : x;
};

export const compilePat = (pat: Pat): string => {
  if (pat.tag === 'PVar') return pat.name;
  if (pat.tag === 'PAnn') return compilePat(pat.pat);
  if (pat.tag === 'PWildcard') return '_';
  if (pat.tag === 'PCon') return compilePat(pat.pat);
  return impossible('compilePat');
};

export const compile = (term: Term): string => {
  if (term.tag === 'Var') return compileName(term.name);
  if (term.tag === 'Abs') return `(${compilePat(term.pat)} => ${compile(term.body)})`;
  if (term.tag === 'App') return `${compile(term.left)}(${compile(term.right)})`;
  if (term.tag === 'Ann') return compile(term.term);
  if (term.tag === 'Let') return `(${compileName(term.name)} => ${compile(term.body)})(${compile(term.val)})`;
  if (term.tag === 'If') return `if_(${compile(term.cond)}, () => ${compile(term.ifTrue)}, () => ${compile(term.ifFalse)})`;
  if (term.tag === 'LitNat') {
    const n = term.val;
    let c = 'z';
    for (let i = 0; i < n; i++) c = `s(${c})`;
    return c;
  }
  if (term.tag === 'LitChar') {
    const n = term.val.charCodeAt(0);
    let c = 'z';
    for (let i = 0; i < n; i++) c = `s(${c})`;
    return `Char(${c})`;
  }
  if (term.tag === 'LitStr') {
    const v = term.val;
    let c = 'nil';
    for (let i = v.length - 1; i >= 0; i--) {
      const char = compile(LitChar(v[i]));
      c = `cons(${char})(${c})`;
    }
    return `Str(${c})`;
  }
  return impossible('compile');
};

export const compileDef = (def: Def, prefix: (name: string) => string): string => {
  switch (def.tag) {
    case 'DType': {
      const con = `${prefix(compileName(def.name))} = x => x;`;
      return `${con}`;
    }
    case 'DLet':
      return `${prefix(compileName(def.name))} = ${def.args.map(compilePat).join(' => ')}${def.args.length > 0 ? ' => ' : ''}${compile(def.term)};`;
  }
};
export const compileDefs = (ds: Def[], prefix: (name: string) => string): string =>
  ds.map(d => compileDef(d, prefix)).join('\n') + '\n';

const keywords = `
do
if
in
for
let
new
try
var
case
else
enum
eval
null
this
true
void
with
await
break
catch
class
const
false
super
throw
while
yield
delete
export
import
public
return
static
switch
typeof
default
extends
finally
package
private
continue
debugger
function
arguments
interface
protected
implements
instanceof
`.trim().split(/\s+/);

