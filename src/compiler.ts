import { Term } from './terms';
import { showName, NameT } from './names';

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
