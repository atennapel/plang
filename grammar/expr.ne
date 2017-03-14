@include "type.ne"
@builtin "whitespace.ne"
@builtin "number.ne"
@builtin "string.ne"

@{%
  var KEYWORDS = [
    'if',
    'then',
    'else',

    'let',

    'case',
    'handle'
  ];
%}

Expr ->
  ExprR _ ":" _ Type {% d => ({tag: 'EAnno', expr: d[0], type: d[4]}) %}
  | ExprR {% d => d[0] %}

ExprR ->
  "if" _ ExprRR _ "then" _ ExprR _ "else" _ ExprR
    {% d => ({tag: 'EIf', cond: d[2], caseTrue: d[6], caseFalse: d[10]}) %}
  | "case" _ (ExprRR _):? "{" _ name _ (name _):* "->" _ Expr (_ ";" _ name _ (name _):* "->" _ Expr):* _ "}"
    {% d => ({
      tag: 'ECase',
      expr: d[2]? d[2][0]: null,
      cases: [{constructor: d[5], args: d[7].map(x => x[0]), expr: d[10]}].concat(
        d[11].map(x => ({constructor: x[3], args: x[5].map(x => x[0]), expr: x[8]}))
      ),
    }) %}
  | "handle" _ (ExprRR _):? "{" _ name _ (name _):* "->" _ Expr (_ ";" _ name _ (name _):* "->" _ Expr):* _ "}"
    {% d => ({
      tag: 'EHandle',
      expr: d[2]? d[2][0]: null,
      cases: [{constructor: d[5], args: d[7].map(x => x[0]), expr: d[10]}].concat(
        d[11].map(x => ({constructor: x[3], args: x[5].map(x => x[0]), expr: x[8]}))
      ),
    }) %}
  | ExprRR {% d => d[0] %}

ExprRR ->
  ExprRR __ ExprRRR
    {% d => ({tag: 'EApp', left: d[0], right: d[2]}) %}
  | ExprRRR {% d => d[0] %}

ExprRRR ->
  "(" _ ")" {% d => ({tag: 'ETuple', vals: []}) %}
  | "{" _ "}" {% d => ({tag: 'ERecord', vals: []}) %}
  | "[" _ "]" {% d => ({tag: 'EList', vals: []}) %}
  | "(" _ Expr (_ "," _ Expr):+ _ ")"
    {% d => ({tag: 'ETuple', vals: [d[2]].concat(d[3].map(x => x[3]))}) %}
  | "[" _ Expr (_ "," _ Expr):* _ "]"
    {% d => ({tag: 'EList', vals: [d[2]].concat(d[3].map(x => x[3]))}) %}
  | "{" _ label (_ "=" _ Expr):? (_ "," _ label (_ "=" _ Expr):?):* _ "}"
    {% d => ({
      tag: 'ERecord',
      vals: [
        [d[2], d[3]? d[3][3]: null]
      ].concat(d[4].map(x => [x[3], x[4]? x[4][3]: null])),
    }) %}
  | "\\" (_ Arg):+ _ "->" _ ExprR
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
  [A-Za-z] [A-Za-z0-9]:* {%
    function(d, location, reject) {
      var name = d[0] + d[1].join('');
      if(KEYWORDS.indexOf(name) === -1) {
        return name;
      } else {
        return reject;
      }
    }
  %}

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
## handle,
## iff,
## casee,
## list,
# iapp, e {e}
# eapp, e {{e}}
# lete,
# letr,
# ilet,
# iletr,
# doe (x <- y; z)
# ;
# RecordSelect,
# recordselect,
# RecordExtend,
# recordextend,
# RecordRestrict,
# recordrestrict,
# RecordUpdate,
# recordupdate,
# perform,
