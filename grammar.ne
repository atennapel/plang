@builtin "whitespace.ne"

main ->
  null |
  Stat (__ Stat):* {% d => [d[0]].concat(d[1].map(x => x[1])) %}

Stat ->
  Def {% d => d[0] %}
  | Dir {% d => d[0] %}
  | Anno {% d => d[0] %}

Dir ->
  "%" name {% d => ({tag: 'SDir', name: d[1]}) %}

Anno ->
  name _ ":" _ Type {% d => ({tag: 'SAnno', name: d[0], type: d[4]}) %}

Def ->
  name (__ Arg):* _ "=" _ Expr
    {% d => ({tag: 'SDef', name: d[0], args: d[1].map(x => x[1]), val: d[5]}) %}

Arg ->
  name {% d => ({tag: 'ArgU', name: d[0]}) %}
  | "{" _ name _ "}" {% d => ({tag: 'ArgIU', name: d[2]}) %}
  | "(" _ name _ ":" _ Type _ ")"
    {% d => ({tag: 'ArgA', name: d[2], type: d[6]}) %}
  | "{" _ name _ ":" _ Type _ "}"
    {% d => ({tag: 'ArgIA', name: d[2], type: d[6]}) %}

Type ->
  [a-z] [a-zA-Z0-9]:* {% d => ({tag: 'TVar', name: d[0] + d[1].join('')}) %}
  | [A-Z] [a-zA-Z0-9]:* {% d => ({tag: 'TCon', name: d[0] + d[1].join('')}) %}
  | "(" _ Type __ Type _ ")" {% d => ({tag: 'TApp', left: d[2], right: d[4]}) %}
  | Type __ Type {% d => ({tag: 'TApp', left: d[0], right: d[2]}) %}

Expr ->
  name {% d => ({tag: 'EVar', val: d[0]}) %}
  | "(" _ Expr _ ")" {% d => d[1] %}

name ->
  [_a-zA-Z] [_a-zA-Z0-9]:* {% d => d[0] + d[1].join('') %}
