import { Term } from './terms';
import { showName, NameT } from './names';
import { Def } from './definitions';

export const compileName = (name: NameT) => {
  const x = showName(name);
  return keywords.indexOf(x) >= 0 ? `${x}_` : x;
};

export const compile = (term: Term): string => {
  switch (term.tag) {
    case 'Var': return compileName(term.name);
    case 'Abs': return `(${compileName(term.name)} => ${compile(term.body)})`;
    case 'App': return `${compile(term.left)}(${compile(term.right)})`;
    case 'Ann': return compile(term.term);
    case 'Let': return `(${compileName(term.name)} => ${compile(term.body)})(${compile(term.term)})`;
  }
};

export const compileDef = (def: Def, prefix: (name: string) => string): string => {
  switch (def.tag) {
    case 'DType': {
      const con = `${prefix(compileName(def.name))} = x => x;`;
      const uncon = `${prefix(`un${compileName(def.name)}`)} = x => x;`;
      return `${con}\n${uncon}`;
    }
    case 'DLet':
      return `${prefix(compileName(def.name))} = ${def.args.map(compileName).join(' => ')}${def.args.length > 0 ? ' => ' : ''}${compile(def.term)};`;
    case 'DDeclType': return '';
    case 'DDeclare': return '';
    case 'DForeign': return `${prefix(compileName(def.name))} = ${def.val};`;
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
