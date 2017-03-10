@builtin "whitespace.ne"

Type ->
  "{" _ TypeR _ "}" _ "->" _ Type
    {% d => ({tag: 'TImpl', left: d[2], right: d[8]}) %}
  | TypeR _ "->" _ Type {% d => ({tag: 'TArr', left: d[0], right: d[4]}) %}
  | TypeR {% d => d[0] %}

TypeR ->
  TypeR __ TypeRR {% d => ({tag: 'TApp', left: d[0], right: d[2]}) %}
  | TypeRR {% d => d[0] %}

TypeRR ->
  "(" _ ")" {% d => ({tag: 'TTuple', vals: []}) %}
  | "{" _ "}" {% d => ({tag: 'TRow', vals: [], tvar: null}) %}
  | TVar {% d => d[0] %}
  | TCon {% d => d[0] %}
  | "(" _ Type _ ")" {% d => d[2] %}
  | "(" _ Type (_ "," _ Type):+ _ ")"
    {% d => ({tag: 'TTuple', vals: [d[2]].concat(d[3].map(x => x[3]))}) %}
  | "{" _ label _ ":" _ Type (_ "," _ label _ ":" _ Type):* _ ("|" _ tvar _):? "}"
    {% d => ({
      tag: 'TRow',
      vals: [[d[2], d[6]]].concat(d[7].map(x => [x[3], x[7]])),
      tvar: d[9]? d[9][2]: null,
    }) %}

TCon -> tcon {% d => ({tag: 'TCon', name: d[0]}) %}
tcon ->
  [A-Z] [A-Za-z0-9]:* {% d => d[0] + d[1].join('') %}

TVar -> tvar {% d => ({tag: 'TVar', name: d[0]}) %}
tvar ->
  "'":? [a-z] [A-Za-z0-9]:* {% d => (d[0] || '') + d[1] + d[2].join('') %}

label ->
  [A-Za-z] [A-Za-z0-9]:* {% d => d[0] + d[1].join('') %}
