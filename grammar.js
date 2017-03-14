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
    {"name": "main", "symbols": []},
    {"name": "main$ebnf$1", "symbols": []},
    {"name": "main$ebnf$1$subexpression$1", "symbols": ["__", "Stat"]},
    {"name": "main$ebnf$1", "symbols": ["main$ebnf$1", "main$ebnf$1$subexpression$1"], "postprocess": function arrpush(d) {return d[0].concat([d[1]]);}},
    {"name": "main", "symbols": ["Stat", "main$ebnf$1"], "postprocess": d => [d[0]].concat(d[1].map(x => x[1]))},
    {"name": "Stat", "symbols": ["Def"], "postprocess": d => d[0]},
    {"name": "Stat", "symbols": ["Dir"], "postprocess": d => d[0]},
    {"name": "Stat", "symbols": ["Anno"], "postprocess": d => d[0]},
    {"name": "Dir", "symbols": [{"literal":"%"}, "name"], "postprocess": d => ({tag: 'SDir', name: d[1]})},
    {"name": "Anno", "symbols": ["name", "_", {"literal":":"}, "_", "Type"], "postprocess": d => ({tag: 'SAnno', name: d[0], type: d[4]})},
    {"name": "Def$ebnf$1", "symbols": []},
    {"name": "Def$ebnf$1$subexpression$1", "symbols": ["__", "Arg"]},
    {"name": "Def$ebnf$1", "symbols": ["Def$ebnf$1", "Def$ebnf$1$subexpression$1"], "postprocess": function arrpush(d) {return d[0].concat([d[1]]);}},
    {"name": "Def", "symbols": ["name", "Def$ebnf$1", "_", {"literal":"="}, "_", "Expr"], "postprocess": d => ({tag: 'SDef', name: d[0], args: d[1].map(x => x[1]), val: d[5]})},
    {"name": "Arg", "symbols": ["name"], "postprocess": d => ({tag: 'ArgU', name: d[0]})},
    {"name": "Arg", "symbols": [{"literal":"{"}, "_", "name", "_", {"literal":"}"}], "postprocess": d => ({tag: 'ArgIU', name: d[2]})},
    {"name": "Arg", "symbols": [{"literal":"("}, "_", "name", "_", {"literal":":"}, "_", "Type", "_", {"literal":")"}], "postprocess": d => ({tag: 'ArgA', name: d[2], type: d[6]})},
    {"name": "Arg", "symbols": [{"literal":"{"}, "_", "name", "_", {"literal":":"}, "_", "Type", "_", {"literal":"}"}], "postprocess": d => ({tag: 'ArgIA', name: d[2], type: d[6]})},
    {"name": "Type$ebnf$1", "symbols": []},
    {"name": "Type$ebnf$1", "symbols": ["Type$ebnf$1", /[a-zA-Z0-9]/], "postprocess": function arrpush(d) {return d[0].concat([d[1]]);}},
    {"name": "Type", "symbols": [/[a-z]/, "Type$ebnf$1"], "postprocess": d => ({tag: 'TVar', name: d[0] + d[1].join('')})},
    {"name": "Type$ebnf$2", "symbols": []},
    {"name": "Type$ebnf$2", "symbols": ["Type$ebnf$2", /[a-zA-Z0-9]/], "postprocess": function arrpush(d) {return d[0].concat([d[1]]);}},
    {"name": "Type", "symbols": [/[A-Z]/, "Type$ebnf$2"], "postprocess": d => ({tag: 'TCon', name: d[0] + d[1].join('')})},
    {"name": "Type", "symbols": [{"literal":"("}, "_", "Type", "__", "Type", "_", {"literal":")"}], "postprocess": d => ({tag: 'TApp', left: d[2], right: d[4]})},
    {"name": "Type", "symbols": ["Type", "__", "Type"], "postprocess": d => ({tag: 'TApp', left: d[0], right: d[2]})},
    {"name": "Expr", "symbols": ["name"], "postprocess": d => ({tag: 'EVar', val: d[0]})},
    {"name": "Expr", "symbols": [{"literal":"("}, "_", "Expr", "_", {"literal":")"}], "postprocess": d => d[1]},
    {"name": "name$ebnf$1", "symbols": []},
    {"name": "name$ebnf$1", "symbols": ["name$ebnf$1", /[_a-zA-Z0-9]/], "postprocess": function arrpush(d) {return d[0].concat([d[1]]);}},
    {"name": "name", "symbols": [/[_a-zA-Z]/, "name$ebnf$1"], "postprocess": d => d[0] + d[1].join('')}
]
  , ParserStart: "main"
}
if (typeof module !== 'undefined'&& typeof module.exports !== 'undefined') {
   module.exports = grammar;
} else {
   window.grammar = grammar;
}
})();
