@include "type.ne"
@builtin "whitespace.ne"
@builtin "number.ne"
@builtin "string.ne"

Expr ->
  ExprR _ ":" _ Type {% d => ({tag: 'EAnno', expr: d[0], type: d[4]}) %}
  | ExprR {% d => d[0] %}

ExprR ->
  ExprR __ ExprRR {% d => ({tag: 'EApp', left: d[0], right: d[2]}) %}
  | ExprRR {% d => d[0] %}

ExprRR ->
  "(" _ ")" {% d => ({tag: 'ETuple', vals: []}) %}
  | "{" _ "}" {% d => ({tag: 'ERecord', vals: []}) %}
  | "(" _ Type (_ "," _ Type):+ _ ")"
    {% d => ({tag: 'ETuple', vals: [d[2]].concat(d[3].map(x => x[3]))}) %}
  | "{" _ label (_ "=" _ Expr):? (_ "," _ label (_ "=" _ Expr):?):* _ "}"
    {% d => ({
      tag: 'ERecord',
      vals: [
        [d[2], d[3]? d[3][3]: {tag: 'EVar', name: d[2]}]
      ].concat(d[4].map(x => [x[3], x[4]? x[4][3]: {tag: 'EVar', name: x[3]}])),
    }) %}
  | "\\" (_ Arg):+ _ "->" _ Expr
    {% d => ({tag: 'ELambda', args: d[1].map(x => x[1]), body: d[5]}) %}
  | name {% d => ({tag: 'EVar', name: d[0]}) %}
  | jsonfloat {% d => ({tag: 'ENumber', val: d[0]}) %}
  | dqstring {% d => ({tag: 'EString', val: d[0]}) %}
  | "(" _ Expr _ ")" {% d => d[2] %}

Arg ->
  name {% d => ({tag: 'ArgName', name: d[0], implicit: false}) %}
  | "(" _ name _ ":" _ Type _ ")"
    {% d => ({tag: 'ArgAnno', name: d[2], type: d[6], implicit: false}) %}
  | "{" _ name _ "}"
    {% d => ({tag: 'ArgName', name: d[2], implicit: true}) %}
  | "{" _ name _ ":" _ Type _ "}"
    {% d => ({tag: 'ArgAnno', name: d[2], type: d[6], implicit: true}) %}

name ->
  [A-Za-z] [A-Za-z0-9]:* {% d => d[0] + d[1].join('') %}

## e : t
## e e
## var
## ()
## {}
## float
## int
## string
## record
## tuple
## lambda
## implicit lambda
# let
# do
# ;
