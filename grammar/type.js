// Generated automatically by nearley
// http://github.com/Hardmath123/nearley
(function () {
function id(x) {return x[0]; }
var grammar = {
    ParserRules: [
    {"name": "_$ebnf$1", "symbols": []},
    {"name": "_$ebnf$1", "symbols": ["_$ebnf$1", "wschar"], "postprocess": function arrpush(d) {return d[0].concat([d[1]]);}},
    {"name": "_", "symbols": ["_$ebnf$1"], "postprocess": function(d) {return null;}},
    {"name": "__$ebnf$1", "symbols": ["wschar"]},
    {"name": "__$ebnf$1", "symbols": ["__$ebnf$1", "wschar"], "postprocess": function arrpush(d) {return d[0].concat([d[1]]);}},
    {"name": "__", "symbols": ["__$ebnf$1"], "postprocess": function(d) {return null;}},
    {"name": "wschar", "symbols": [/[ \t\n\v\f]/], "postprocess": id},
    {"name": "main", "symbols": ["Type"], "postprocess": d => d[0]},
    {"name": "Type$string$1", "symbols": [{"literal":"-"}, {"literal":">"}], "postprocess": function joiner(d) {return d.join('');}},
    {"name": "Type", "symbols": [{"literal":"{"}, "_", "TypeR", "_", {"literal":"}"}, "_", "Type$string$1", "_", "Type"], "postprocess": d => ({tag: 'TImpl', left: d[2], right: d[8]})},
    {"name": "Type$string$2", "symbols": [{"literal":"-"}, {"literal":">"}], "postprocess": function joiner(d) {return d.join('');}},
    {"name": "Type", "symbols": ["TypeR", "_", "Type$string$2", "_", "Type"], "postprocess": d => ({tag: 'TArr', left: d[0], right: d[4]})},
    {"name": "Type", "symbols": ["TypeR"], "postprocess": d => d[0]},
    {"name": "TypeR", "symbols": ["TypeR", "__", "TypeRR"], "postprocess": d => ({tag: 'TApp', left: d[0], right: d[2]})},
    {"name": "TypeR", "symbols": ["TypeRR"], "postprocess": d => d[0]},
    {"name": "TypeRR$string$1", "symbols": [{"literal":"("}, {"literal":")"}], "postprocess": function joiner(d) {return d.join('');}},
    {"name": "TypeRR", "symbols": ["TypeRR$string$1"], "postprocess": d => ({tag: 'TTuple', vals: []})},
    {"name": "TypeRR$string$2", "symbols": [{"literal":"{"}, {"literal":"}"}], "postprocess": function joiner(d) {return d.join('');}},
    {"name": "TypeRR", "symbols": ["TypeRR$string$2"], "postprocess": d => ({tag: 'TRow', vals: [], tvar: null})},
    {"name": "TypeRR", "symbols": ["TVar"], "postprocess": d => d[0]},
    {"name": "TypeRR", "symbols": ["TCon"], "postprocess": d => d[0]},
    {"name": "TypeRR", "symbols": [{"literal":"("}, "_", "Type", "_", {"literal":")"}], "postprocess": d => d[2]},
    {"name": "TypeRR$ebnf$1$subexpression$1", "symbols": ["_", {"literal":","}, "_", "Type"]},
    {"name": "TypeRR$ebnf$1", "symbols": ["TypeRR$ebnf$1$subexpression$1"]},
    {"name": "TypeRR$ebnf$1$subexpression$2", "symbols": ["_", {"literal":","}, "_", "Type"]},
    {"name": "TypeRR$ebnf$1", "symbols": ["TypeRR$ebnf$1", "TypeRR$ebnf$1$subexpression$2"], "postprocess": function arrpush(d) {return d[0].concat([d[1]]);}},
    {"name": "TypeRR", "symbols": [{"literal":"("}, "_", "Type", "TypeRR$ebnf$1", "_", {"literal":")"}], "postprocess": d => ({tag: 'TTuple', vals: [d[2]].concat(d[3].map(x => x[3]))})},
    {"name": "TypeRR$ebnf$2", "symbols": []},
    {"name": "TypeRR$ebnf$2$subexpression$1", "symbols": ["_", {"literal":","}, "_", "label", "_", {"literal":":"}, "_", "Type"]},
    {"name": "TypeRR$ebnf$2", "symbols": ["TypeRR$ebnf$2", "TypeRR$ebnf$2$subexpression$1"], "postprocess": function arrpush(d) {return d[0].concat([d[1]]);}},
    {"name": "TypeRR$ebnf$3$subexpression$1", "symbols": [{"literal":"|"}, "_", "tvar", "_"]},
    {"name": "TypeRR$ebnf$3", "symbols": ["TypeRR$ebnf$3$subexpression$1"], "postprocess": id},
    {"name": "TypeRR$ebnf$3", "symbols": [], "postprocess": function(d) {return null;}},
    {"name": "TypeRR", "symbols": [{"literal":"{"}, "_", "label", "_", {"literal":":"}, "_", "Type", "TypeRR$ebnf$2", "_", "TypeRR$ebnf$3", {"literal":"}"}], "postprocess":  d => ({
          tag: 'TRow',
          vals: [[d[2], d[6]]].concat(d[7].map(x => [x[3], x[7]])),
          tvar: d[9]? d[9][2]: null,
        }) },
    {"name": "TCon", "symbols": ["tcon"], "postprocess": d => ({tag: 'TCon', name: d[0]})},
    {"name": "tcon$ebnf$1", "symbols": []},
    {"name": "tcon$ebnf$1", "symbols": ["tcon$ebnf$1", /[A-Za-z0-9]/], "postprocess": function arrpush(d) {return d[0].concat([d[1]]);}},
    {"name": "tcon", "symbols": [/[A-Z]/, "tcon$ebnf$1"], "postprocess": d => d[0] + d[1].join('')},
    {"name": "TVar", "symbols": ["tvar"], "postprocess": d => ({tag: 'TVar', name: d[0]})},
    {"name": "tvar$ebnf$1", "symbols": [{"literal":"'"}], "postprocess": id},
    {"name": "tvar$ebnf$1", "symbols": [], "postprocess": function(d) {return null;}},
    {"name": "tvar$ebnf$2", "symbols": []},
    {"name": "tvar$ebnf$2", "symbols": ["tvar$ebnf$2", /[A-Za-z0-9]/], "postprocess": function arrpush(d) {return d[0].concat([d[1]]);}},
    {"name": "tvar", "symbols": ["tvar$ebnf$1", /[a-z]/, "tvar$ebnf$2"], "postprocess": d => (d[0] || '') + d[1] + d[2].join('')},
    {"name": "label$ebnf$1", "symbols": []},
    {"name": "label$ebnf$1", "symbols": ["label$ebnf$1", /[A-Za-z0-9]/], "postprocess": function arrpush(d) {return d[0].concat([d[1]]);}},
    {"name": "label", "symbols": [/[A-Za-z]/, "label$ebnf$1"], "postprocess": d => d[0] + d[1].join('')}
]
  , ParserStart: "main"
}
if (typeof module !== 'undefined'&& typeof module.exports !== 'undefined') {
   module.exports = grammar;
} else {
   window.grammar = grammar;
}
})();
