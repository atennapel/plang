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

const compileName = x =>
  keywords.indexOf(x) >= 0 ? `${x}_` : /^[0-9]+$/.test(x) ? `_${x}` : x.replace(/\./g, '_');

const compile = e => {
  switch (e.tag) {
    case 'Var': return compileName(e.name);
    case 'Abs':
      return `(${compileName(e.name)} => ${compile(e.body)})`;
    case 'App':
      return `${compile(e.left)}(${compile(e.right)})`;
    case 'Con': return compile(e.arg);
    case 'Decon':
      return `(${compileName(e.name)} => ${compile(e.body)})`;
    case 'LitStr':
      return JSON.stringify(e.val);
  }
};

const compileDef = (d, glob = null) => {
  switch (d.tag) {
    case 'DType': return null;
    case 'DValue':
      return glob ? `${glob}['${compileName(d.name)}'] = ${compile(d.expr)};` : `const ${compileName(d.name)} = ${compile(d.expr)};`;
    case 'DDeclare': return null;
    case 'DForeign':
      return glob ? `${glob}['${compileName(d.name)}'] = ${d.val};` : `const ${compileName(d.name)} = ${d.val};`;
  }
};
const compileDefs = (ds, glob = null) =>
  ds.map(d => compileDef(d, glob))
    .filter(x => x !== null)
    .join('\n');

module.exports = {
  compileDefs,
  compile,
};
