@include "type.ne"
@include "expr.ne"

@builtin "whitespace.ne"

Definition ->
  (Def _ ";"):* Def
    {% d => d[0].map(x => x[0]).concat(d[1]) %}

Def ->
  name _ ":" _ Type
    {% d => ({tag: 'DType', name: d[0], type: d[4]}) %}
  | name _ "=" _ Expr
    {% d => ({tag: 'DDef', name: d[0], val: d[4]}) %}
