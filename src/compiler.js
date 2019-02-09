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

const compileName = x => keywords.indexOf(x) >= 0 ? `${x}_` : x;

const compile = e => {
  switch (e.tag) {
    case 'Var': return compileName(e.name);
    case 'Abs': return `(${compileName(e.name)} => ${compile(e.body)})`;
    case 'App':
      return `${compile(e.left)}(${compile(e.right)})`;
  }
};

const compileDefs = ds =>
  ds.map(([x, e]) => `const ${compileName(x)} = ${compile(e)};`)
    .join('\n');

module.exports = {
  compileDefs,
};
