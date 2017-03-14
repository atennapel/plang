// Generated automatically by nearley
// http://github.com/Hardmath123/nearley
(function () {
function id(x) {return x[0]; }

function nth(n) {
    return function(d) {
        return d[n];
    };
}


function $(o) {
    return function(d) {
        var ret = {};
        Object.keys(o).forEach(function(k) {
            ret[k] = d[o[k]];
        });
        return ret;
    };
}


  var KEYWORDS = [
    'if',
    'then',
    'else',

    'let',

    'case',
    'handle'
  ];
var grammar = {
    ParserRules: [
    {"name": "_$ebnf$1", "symbols": []},
    {"name": "_$ebnf$1", "symbols": ["_$ebnf$1", "wschar"], "postprocess": function arrpush(d) {return d[0].concat([d[1]]);}},
    {"name": "_", "symbols": ["_$ebnf$1"], "postprocess": function(d) {return null;}},
    {"name": "__$ebnf$1", "symbols": ["wschar"]},
    {"name": "__$ebnf$1", "symbols": ["__$ebnf$1", "wschar"], "postprocess": function arrpush(d) {return d[0].concat([d[1]]);}},
    {"name": "__", "symbols": ["__$ebnf$1"], "postprocess": function(d) {return null;}},
    {"name": "wschar", "symbols": [/[ \t\n\v\f]/], "postprocess": id},
    {"name": "Type$string$1", "symbols": [{"literal":"-"}, {"literal":">"}], "postprocess": function joiner(d) {return d.join('');}},
    {"name": "Type", "symbols": [{"literal":"{"}, "_", "TypeR", "_", {"literal":"}"}, "_", "Type$string$1", "_", "Type"], "postprocess": d => ({tag: 'TImpl', left: d[2], right: d[8]})},
    {"name": "Type$string$2", "symbols": [{"literal":"-"}, {"literal":">"}], "postprocess": function joiner(d) {return d.join('');}},
    {"name": "Type", "symbols": ["TypeR", "_", "Type$string$2", "_", "Type"], "postprocess": d => ({tag: 'TArr', left: d[0], right: d[4]})},
    {"name": "Type", "symbols": ["TypeR"], "postprocess": d => d[0]},
    {"name": "TypeR", "symbols": ["TypeR", "__", "TypeRR"], "postprocess": d => ({tag: 'TApp', left: d[0], right: d[2]})},
    {"name": "TypeR", "symbols": ["TypeRR"], "postprocess": d => d[0]},
    {"name": "TypeRR", "symbols": [{"literal":"("}, "_", {"literal":")"}], "postprocess": d => ({tag: 'TTuple', vals: []})},
    {"name": "TypeRR", "symbols": [{"literal":"{"}, "_", {"literal":"}"}], "postprocess": d => ({tag: 'TRow', vals: [], tvar: null})},
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
    {"name": "label", "symbols": [/[A-Za-z]/, "label$ebnf$1"], "postprocess": d => d[0] + d[1].join('')},
    {"name": "unsigned_int$ebnf$1", "symbols": [/[0-9]/]},
    {"name": "unsigned_int$ebnf$1", "symbols": ["unsigned_int$ebnf$1", /[0-9]/], "postprocess": function arrpush(d) {return d[0].concat([d[1]]);}},
    {"name": "unsigned_int", "symbols": ["unsigned_int$ebnf$1"], "postprocess": 
        function(d) {
            return parseInt(d[0].join(""));
        }
        },
    {"name": "int$ebnf$1$subexpression$1", "symbols": [{"literal":"-"}]},
    {"name": "int$ebnf$1$subexpression$1", "symbols": [{"literal":"+"}]},
    {"name": "int$ebnf$1", "symbols": ["int$ebnf$1$subexpression$1"], "postprocess": id},
    {"name": "int$ebnf$1", "symbols": [], "postprocess": function(d) {return null;}},
    {"name": "int$ebnf$2", "symbols": [/[0-9]/]},
    {"name": "int$ebnf$2", "symbols": ["int$ebnf$2", /[0-9]/], "postprocess": function arrpush(d) {return d[0].concat([d[1]]);}},
    {"name": "int", "symbols": ["int$ebnf$1", "int$ebnf$2"], "postprocess": 
        function(d) {
            if (d[0]) {
                return parseInt(d[0][0]+d[1].join(""));
            } else {
                return parseInt(d[1].join(""));
            }
        }
        },
    {"name": "unsigned_decimal$ebnf$1", "symbols": [/[0-9]/]},
    {"name": "unsigned_decimal$ebnf$1", "symbols": ["unsigned_decimal$ebnf$1", /[0-9]/], "postprocess": function arrpush(d) {return d[0].concat([d[1]]);}},
    {"name": "unsigned_decimal$ebnf$2$subexpression$1$ebnf$1", "symbols": [/[0-9]/]},
    {"name": "unsigned_decimal$ebnf$2$subexpression$1$ebnf$1", "symbols": ["unsigned_decimal$ebnf$2$subexpression$1$ebnf$1", /[0-9]/], "postprocess": function arrpush(d) {return d[0].concat([d[1]]);}},
    {"name": "unsigned_decimal$ebnf$2$subexpression$1", "symbols": [{"literal":"."}, "unsigned_decimal$ebnf$2$subexpression$1$ebnf$1"]},
    {"name": "unsigned_decimal$ebnf$2", "symbols": ["unsigned_decimal$ebnf$2$subexpression$1"], "postprocess": id},
    {"name": "unsigned_decimal$ebnf$2", "symbols": [], "postprocess": function(d) {return null;}},
    {"name": "unsigned_decimal", "symbols": ["unsigned_decimal$ebnf$1", "unsigned_decimal$ebnf$2"], "postprocess": 
        function(d) {
            return parseFloat(
                d[0].join("") +
                (d[1] ? "."+d[1][1].join("") : "")
            );
        }
        },
    {"name": "decimal$ebnf$1", "symbols": [{"literal":"-"}], "postprocess": id},
    {"name": "decimal$ebnf$1", "symbols": [], "postprocess": function(d) {return null;}},
    {"name": "decimal$ebnf$2", "symbols": [/[0-9]/]},
    {"name": "decimal$ebnf$2", "symbols": ["decimal$ebnf$2", /[0-9]/], "postprocess": function arrpush(d) {return d[0].concat([d[1]]);}},
    {"name": "decimal$ebnf$3$subexpression$1$ebnf$1", "symbols": [/[0-9]/]},
    {"name": "decimal$ebnf$3$subexpression$1$ebnf$1", "symbols": ["decimal$ebnf$3$subexpression$1$ebnf$1", /[0-9]/], "postprocess": function arrpush(d) {return d[0].concat([d[1]]);}},
    {"name": "decimal$ebnf$3$subexpression$1", "symbols": [{"literal":"."}, "decimal$ebnf$3$subexpression$1$ebnf$1"]},
    {"name": "decimal$ebnf$3", "symbols": ["decimal$ebnf$3$subexpression$1"], "postprocess": id},
    {"name": "decimal$ebnf$3", "symbols": [], "postprocess": function(d) {return null;}},
    {"name": "decimal", "symbols": ["decimal$ebnf$1", "decimal$ebnf$2", "decimal$ebnf$3"], "postprocess": 
        function(d) {
            return parseFloat(
                (d[0] || "") +
                d[1].join("") +
                (d[2] ? "."+d[2][1].join("") : "")
            );
        }
        },
    {"name": "percentage", "symbols": ["decimal", {"literal":"%"}], "postprocess": 
        function(d) {
            return d[0]/100;
        }
        },
    {"name": "jsonfloat$ebnf$1", "symbols": [{"literal":"-"}], "postprocess": id},
    {"name": "jsonfloat$ebnf$1", "symbols": [], "postprocess": function(d) {return null;}},
    {"name": "jsonfloat$ebnf$2", "symbols": [/[0-9]/]},
    {"name": "jsonfloat$ebnf$2", "symbols": ["jsonfloat$ebnf$2", /[0-9]/], "postprocess": function arrpush(d) {return d[0].concat([d[1]]);}},
    {"name": "jsonfloat$ebnf$3$subexpression$1$ebnf$1", "symbols": [/[0-9]/]},
    {"name": "jsonfloat$ebnf$3$subexpression$1$ebnf$1", "symbols": ["jsonfloat$ebnf$3$subexpression$1$ebnf$1", /[0-9]/], "postprocess": function arrpush(d) {return d[0].concat([d[1]]);}},
    {"name": "jsonfloat$ebnf$3$subexpression$1", "symbols": [{"literal":"."}, "jsonfloat$ebnf$3$subexpression$1$ebnf$1"]},
    {"name": "jsonfloat$ebnf$3", "symbols": ["jsonfloat$ebnf$3$subexpression$1"], "postprocess": id},
    {"name": "jsonfloat$ebnf$3", "symbols": [], "postprocess": function(d) {return null;}},
    {"name": "jsonfloat$ebnf$4$subexpression$1$ebnf$1", "symbols": [/[+-]/], "postprocess": id},
    {"name": "jsonfloat$ebnf$4$subexpression$1$ebnf$1", "symbols": [], "postprocess": function(d) {return null;}},
    {"name": "jsonfloat$ebnf$4$subexpression$1$ebnf$2", "symbols": [/[0-9]/]},
    {"name": "jsonfloat$ebnf$4$subexpression$1$ebnf$2", "symbols": ["jsonfloat$ebnf$4$subexpression$1$ebnf$2", /[0-9]/], "postprocess": function arrpush(d) {return d[0].concat([d[1]]);}},
    {"name": "jsonfloat$ebnf$4$subexpression$1", "symbols": [/[eE]/, "jsonfloat$ebnf$4$subexpression$1$ebnf$1", "jsonfloat$ebnf$4$subexpression$1$ebnf$2"]},
    {"name": "jsonfloat$ebnf$4", "symbols": ["jsonfloat$ebnf$4$subexpression$1"], "postprocess": id},
    {"name": "jsonfloat$ebnf$4", "symbols": [], "postprocess": function(d) {return null;}},
    {"name": "jsonfloat", "symbols": ["jsonfloat$ebnf$1", "jsonfloat$ebnf$2", "jsonfloat$ebnf$3", "jsonfloat$ebnf$4"], "postprocess": 
        function(d) {
            return parseFloat(
                (d[0] || "") +
                d[1].join("") +
                (d[2] ? "."+d[2][1].join("") : "") +
                (d[3] ? "e" + (d[3][1] || "+") + d[3][2].join("") : "")
            );
        }
        },
    {"name": "dqstring$ebnf$1", "symbols": []},
    {"name": "dqstring$ebnf$1", "symbols": ["dqstring$ebnf$1", "dstrchar"], "postprocess": function arrpush(d) {return d[0].concat([d[1]]);}},
    {"name": "dqstring", "symbols": [{"literal":"\""}, "dqstring$ebnf$1", {"literal":"\""}], "postprocess": function(d) {return d[1].join(""); }},
    {"name": "sqstring$ebnf$1", "symbols": []},
    {"name": "sqstring$ebnf$1", "symbols": ["sqstring$ebnf$1", "sstrchar"], "postprocess": function arrpush(d) {return d[0].concat([d[1]]);}},
    {"name": "sqstring", "symbols": [{"literal":"'"}, "sqstring$ebnf$1", {"literal":"'"}], "postprocess": function(d) {return d[1].join(""); }},
    {"name": "btstring$ebnf$1", "symbols": []},
    {"name": "btstring$ebnf$1", "symbols": ["btstring$ebnf$1", /[^`]/], "postprocess": function arrpush(d) {return d[0].concat([d[1]]);}},
    {"name": "btstring", "symbols": [{"literal":"`"}, "btstring$ebnf$1", {"literal":"`"}], "postprocess": function(d) {return d[1].join(""); }},
    {"name": "dstrchar", "symbols": [/[^\\"\n]/], "postprocess": id},
    {"name": "dstrchar", "symbols": [{"literal":"\\"}, "strescape"], "postprocess": 
        function(d) {
            return JSON.parse("\""+d.join("")+"\"");
        }
        },
    {"name": "sstrchar", "symbols": [/[^\\\n]/], "postprocess": id},
    {"name": "sstrchar", "symbols": [{"literal":"\\"}, "strescape"], "postprocess": function(d) { return JSON.parse("\""+d.join("")+"\""); }},
    {"name": "sstrchar$string$1", "symbols": [{"literal":"\\"}, {"literal":"'"}], "postprocess": function joiner(d) {return d.join('');}},
    {"name": "sstrchar", "symbols": ["sstrchar$string$1"], "postprocess": function(d) {return "'"; }},
    {"name": "strescape", "symbols": [/["\\/bfnrt]/], "postprocess": id},
    {"name": "strescape", "symbols": [{"literal":"u"}, /[a-fA-F0-9]/, /[a-fA-F0-9]/, /[a-fA-F0-9]/, /[a-fA-F0-9]/], "postprocess": 
        function(d) {
            return d.join("");
        }
        },
    {"name": "Expr", "symbols": ["ExprR", "_", {"literal":":"}, "_", "Type"], "postprocess": d => ({tag: 'EAnno', expr: d[0], type: d[4]})},
    {"name": "Expr", "symbols": ["ExprR"], "postprocess": d => d[0]},
    {"name": "ExprR$string$1", "symbols": [{"literal":"i"}, {"literal":"f"}], "postprocess": function joiner(d) {return d.join('');}},
    {"name": "ExprR$string$2", "symbols": [{"literal":"t"}, {"literal":"h"}, {"literal":"e"}, {"literal":"n"}], "postprocess": function joiner(d) {return d.join('');}},
    {"name": "ExprR$string$3", "symbols": [{"literal":"e"}, {"literal":"l"}, {"literal":"s"}, {"literal":"e"}], "postprocess": function joiner(d) {return d.join('');}},
    {"name": "ExprR", "symbols": ["ExprR$string$1", "_", "ExprRR", "_", "ExprR$string$2", "_", "ExprR", "_", "ExprR$string$3", "_", "ExprR"], "postprocess": d => ({tag: 'EIf', cond: d[2], caseTrue: d[6], caseFalse: d[10]})},
    {"name": "ExprR$string$4", "symbols": [{"literal":"c"}, {"literal":"a"}, {"literal":"s"}, {"literal":"e"}], "postprocess": function joiner(d) {return d.join('');}},
    {"name": "ExprR$ebnf$1$subexpression$1", "symbols": ["ExprRR", "_"]},
    {"name": "ExprR$ebnf$1", "symbols": ["ExprR$ebnf$1$subexpression$1"], "postprocess": id},
    {"name": "ExprR$ebnf$1", "symbols": [], "postprocess": function(d) {return null;}},
    {"name": "ExprR$ebnf$2", "symbols": []},
    {"name": "ExprR$ebnf$2$subexpression$1", "symbols": ["name", "_"]},
    {"name": "ExprR$ebnf$2", "symbols": ["ExprR$ebnf$2", "ExprR$ebnf$2$subexpression$1"], "postprocess": function arrpush(d) {return d[0].concat([d[1]]);}},
    {"name": "ExprR$string$5", "symbols": [{"literal":"-"}, {"literal":">"}], "postprocess": function joiner(d) {return d.join('');}},
    {"name": "ExprR$ebnf$3", "symbols": []},
    {"name": "ExprR$ebnf$3$subexpression$1$ebnf$1", "symbols": []},
    {"name": "ExprR$ebnf$3$subexpression$1$ebnf$1$subexpression$1", "symbols": ["name", "_"]},
    {"name": "ExprR$ebnf$3$subexpression$1$ebnf$1", "symbols": ["ExprR$ebnf$3$subexpression$1$ebnf$1", "ExprR$ebnf$3$subexpression$1$ebnf$1$subexpression$1"], "postprocess": function arrpush(d) {return d[0].concat([d[1]]);}},
    {"name": "ExprR$ebnf$3$subexpression$1$string$1", "symbols": [{"literal":"-"}, {"literal":">"}], "postprocess": function joiner(d) {return d.join('');}},
    {"name": "ExprR$ebnf$3$subexpression$1", "symbols": ["_", {"literal":";"}, "_", "name", "_", "ExprR$ebnf$3$subexpression$1$ebnf$1", "ExprR$ebnf$3$subexpression$1$string$1", "_", "Expr"]},
    {"name": "ExprR$ebnf$3", "symbols": ["ExprR$ebnf$3", "ExprR$ebnf$3$subexpression$1"], "postprocess": function arrpush(d) {return d[0].concat([d[1]]);}},
    {"name": "ExprR", "symbols": ["ExprR$string$4", "_", "ExprR$ebnf$1", {"literal":"{"}, "_", "name", "_", "ExprR$ebnf$2", "ExprR$string$5", "_", "Expr", "ExprR$ebnf$3", "_", {"literal":"}"}], "postprocess":  d => ({
          tag: 'ECase',
          expr: d[2]? d[2][0]: null,
          cases: [{constructor: d[5], args: d[7].map(x => x[0]), expr: d[10]}].concat(
            d[11].map(x => ({constructor: x[3], args: x[5].map(x => x[0]), expr: x[8]}))
          ),
        }) },
    {"name": "ExprR$string$6", "symbols": [{"literal":"h"}, {"literal":"a"}, {"literal":"n"}, {"literal":"d"}, {"literal":"l"}, {"literal":"e"}], "postprocess": function joiner(d) {return d.join('');}},
    {"name": "ExprR$ebnf$4$subexpression$1", "symbols": ["ExprRR", "_"]},
    {"name": "ExprR$ebnf$4", "symbols": ["ExprR$ebnf$4$subexpression$1"], "postprocess": id},
    {"name": "ExprR$ebnf$4", "symbols": [], "postprocess": function(d) {return null;}},
    {"name": "ExprR$ebnf$5", "symbols": []},
    {"name": "ExprR$ebnf$5$subexpression$1", "symbols": ["name", "_"]},
    {"name": "ExprR$ebnf$5", "symbols": ["ExprR$ebnf$5", "ExprR$ebnf$5$subexpression$1"], "postprocess": function arrpush(d) {return d[0].concat([d[1]]);}},
    {"name": "ExprR$string$7", "symbols": [{"literal":"-"}, {"literal":">"}], "postprocess": function joiner(d) {return d.join('');}},
    {"name": "ExprR$ebnf$6", "symbols": []},
    {"name": "ExprR$ebnf$6$subexpression$1$ebnf$1", "symbols": []},
    {"name": "ExprR$ebnf$6$subexpression$1$ebnf$1$subexpression$1", "symbols": ["name", "_"]},
    {"name": "ExprR$ebnf$6$subexpression$1$ebnf$1", "symbols": ["ExprR$ebnf$6$subexpression$1$ebnf$1", "ExprR$ebnf$6$subexpression$1$ebnf$1$subexpression$1"], "postprocess": function arrpush(d) {return d[0].concat([d[1]]);}},
    {"name": "ExprR$ebnf$6$subexpression$1$string$1", "symbols": [{"literal":"-"}, {"literal":">"}], "postprocess": function joiner(d) {return d.join('');}},
    {"name": "ExprR$ebnf$6$subexpression$1", "symbols": ["_", {"literal":";"}, "_", "name", "_", "ExprR$ebnf$6$subexpression$1$ebnf$1", "ExprR$ebnf$6$subexpression$1$string$1", "_", "Expr"]},
    {"name": "ExprR$ebnf$6", "symbols": ["ExprR$ebnf$6", "ExprR$ebnf$6$subexpression$1"], "postprocess": function arrpush(d) {return d[0].concat([d[1]]);}},
    {"name": "ExprR", "symbols": ["ExprR$string$6", "_", "ExprR$ebnf$4", {"literal":"{"}, "_", "name", "_", "ExprR$ebnf$5", "ExprR$string$7", "_", "Expr", "ExprR$ebnf$6", "_", {"literal":"}"}], "postprocess":  d => ({
          tag: 'EHandle',
          expr: d[2]? d[2][0]: null,
          cases: [{constructor: d[5], args: d[7].map(x => x[0]), expr: d[10]}].concat(
            d[11].map(x => ({constructor: x[3], args: x[5].map(x => x[0]), expr: x[8]}))
          ),
        }) },
    {"name": "ExprR", "symbols": ["ExprRR"], "postprocess": d => d[0]},
    {"name": "ExprRR", "symbols": ["ExprRR", "__", "ExprRRR"], "postprocess": d => ({tag: 'EApp', left: d[0], right: d[2]})},
    {"name": "ExprRR", "symbols": ["ExprRRR"], "postprocess": d => d[0]},
    {"name": "ExprRRR", "symbols": [{"literal":"("}, "_", {"literal":")"}], "postprocess": d => ({tag: 'ETuple', vals: []})},
    {"name": "ExprRRR", "symbols": [{"literal":"{"}, "_", {"literal":"}"}], "postprocess": d => ({tag: 'ERecord', vals: []})},
    {"name": "ExprRRR", "symbols": [{"literal":"["}, "_", {"literal":"]"}], "postprocess": d => ({tag: 'EList', vals: []})},
    {"name": "ExprRRR$ebnf$1$subexpression$1", "symbols": ["_", {"literal":","}, "_", "Expr"]},
    {"name": "ExprRRR$ebnf$1", "symbols": ["ExprRRR$ebnf$1$subexpression$1"]},
    {"name": "ExprRRR$ebnf$1$subexpression$2", "symbols": ["_", {"literal":","}, "_", "Expr"]},
    {"name": "ExprRRR$ebnf$1", "symbols": ["ExprRRR$ebnf$1", "ExprRRR$ebnf$1$subexpression$2"], "postprocess": function arrpush(d) {return d[0].concat([d[1]]);}},
    {"name": "ExprRRR", "symbols": [{"literal":"("}, "_", "Expr", "ExprRRR$ebnf$1", "_", {"literal":")"}], "postprocess": d => ({tag: 'ETuple', vals: [d[2]].concat(d[3].map(x => x[3]))})},
    {"name": "ExprRRR$ebnf$2", "symbols": []},
    {"name": "ExprRRR$ebnf$2$subexpression$1", "symbols": ["_", {"literal":","}, "_", "Expr"]},
    {"name": "ExprRRR$ebnf$2", "symbols": ["ExprRRR$ebnf$2", "ExprRRR$ebnf$2$subexpression$1"], "postprocess": function arrpush(d) {return d[0].concat([d[1]]);}},
    {"name": "ExprRRR", "symbols": [{"literal":"["}, "_", "Expr", "ExprRRR$ebnf$2", "_", {"literal":"]"}], "postprocess": d => ({tag: 'EList', vals: [d[2]].concat(d[3].map(x => x[3]))})},
    {"name": "ExprRRR$ebnf$3$subexpression$1", "symbols": ["_", {"literal":"="}, "_", "Expr"]},
    {"name": "ExprRRR$ebnf$3", "symbols": ["ExprRRR$ebnf$3$subexpression$1"], "postprocess": id},
    {"name": "ExprRRR$ebnf$3", "symbols": [], "postprocess": function(d) {return null;}},
    {"name": "ExprRRR$ebnf$4", "symbols": []},
    {"name": "ExprRRR$ebnf$4$subexpression$1$ebnf$1$subexpression$1", "symbols": ["_", {"literal":"="}, "_", "Expr"]},
    {"name": "ExprRRR$ebnf$4$subexpression$1$ebnf$1", "symbols": ["ExprRRR$ebnf$4$subexpression$1$ebnf$1$subexpression$1"], "postprocess": id},
    {"name": "ExprRRR$ebnf$4$subexpression$1$ebnf$1", "symbols": [], "postprocess": function(d) {return null;}},
    {"name": "ExprRRR$ebnf$4$subexpression$1", "symbols": ["_", {"literal":","}, "_", "label", "ExprRRR$ebnf$4$subexpression$1$ebnf$1"]},
    {"name": "ExprRRR$ebnf$4", "symbols": ["ExprRRR$ebnf$4", "ExprRRR$ebnf$4$subexpression$1"], "postprocess": function arrpush(d) {return d[0].concat([d[1]]);}},
    {"name": "ExprRRR", "symbols": [{"literal":"{"}, "_", "label", "ExprRRR$ebnf$3", "ExprRRR$ebnf$4", "_", {"literal":"}"}], "postprocess":  d => ({
          tag: 'ERecord',
          vals: [
            [d[2], d[3]? d[3][3]: null]
          ].concat(d[4].map(x => [x[3], x[4]? x[4][3]: null])),
        }) },
    {"name": "ExprRRR$ebnf$5$subexpression$1", "symbols": ["_", "Arg"]},
    {"name": "ExprRRR$ebnf$5", "symbols": ["ExprRRR$ebnf$5$subexpression$1"]},
    {"name": "ExprRRR$ebnf$5$subexpression$2", "symbols": ["_", "Arg"]},
    {"name": "ExprRRR$ebnf$5", "symbols": ["ExprRRR$ebnf$5", "ExprRRR$ebnf$5$subexpression$2"], "postprocess": function arrpush(d) {return d[0].concat([d[1]]);}},
    {"name": "ExprRRR$string$1", "symbols": [{"literal":"-"}, {"literal":">"}], "postprocess": function joiner(d) {return d.join('');}},
    {"name": "ExprRRR", "symbols": [{"literal":"\\"}, "ExprRRR$ebnf$5", "_", "ExprRRR$string$1", "_", "ExprR"], "postprocess": d => ({tag: 'ELambda', args: d[1].map(x => x[1]), body: d[5]})},
    {"name": "ExprRRR", "symbols": ["name"], "postprocess": d => ({tag: 'EVar', name: d[0]})},
    {"name": "ExprRRR", "symbols": ["jsonfloat"], "postprocess": d => ({tag: 'ENumber', val: d[0]})},
    {"name": "ExprRRR", "symbols": ["dqstring"], "postprocess": d => ({tag: 'EString', val: d[0]})},
    {"name": "ExprRRR", "symbols": [{"literal":"("}, "_", "Expr", "_", {"literal":")"}], "postprocess": d => d[2]},
    {"name": "Arg", "symbols": ["name"], "postprocess": d => ({tag: 'ArgName', name: d[0], implicit: false})},
    {"name": "Arg", "symbols": [{"literal":"("}, "_", "name", "_", {"literal":":"}, "_", "Type", "_", {"literal":")"}], "postprocess": d => ({tag: 'ArgAnno', name: d[2], type: d[6], implicit: false})},
    {"name": "Arg", "symbols": [{"literal":"{"}, "_", "name", "_", {"literal":"}"}], "postprocess": d => ({tag: 'ArgName', name: d[2], implicit: true})},
    {"name": "Arg", "symbols": [{"literal":"{"}, "_", "name", "_", {"literal":":"}, "_", "Type", "_", {"literal":"}"}], "postprocess": d => ({tag: 'ArgAnno', name: d[2], type: d[6], implicit: true})},
    {"name": "name$ebnf$1", "symbols": []},
    {"name": "name$ebnf$1", "symbols": ["name$ebnf$1", /[A-Za-z0-9]/], "postprocess": function arrpush(d) {return d[0].concat([d[1]]);}},
    {"name": "name", "symbols": [/[A-Za-z]/, "name$ebnf$1"], "postprocess": 
        function(d, location, reject) {
          var name = d[0] + d[1].join('');
          if(KEYWORDS.indexOf(name) === -1) {
            return name;
          } else {
            return reject;
          }
        }
          },
    {"name": "Definition$ebnf$1", "symbols": []},
    {"name": "Definition$ebnf$1$subexpression$1", "symbols": ["Def", "_", {"literal":";"}]},
    {"name": "Definition$ebnf$1", "symbols": ["Definition$ebnf$1", "Definition$ebnf$1$subexpression$1"], "postprocess": function arrpush(d) {return d[0].concat([d[1]]);}},
    {"name": "Definition", "symbols": ["Definition$ebnf$1", "Def"], "postprocess": d => d[0].map(x => x[0]).concat(d[1])},
    {"name": "Def", "symbols": ["name", "_", {"literal":":"}, "_", "Type"], "postprocess": d => ({tag: 'DType', name: d[0], type: d[4]})},
    {"name": "Def", "symbols": ["name", "_", {"literal":"="}, "_", "Expr"], "postprocess": d => ({tag: 'DDef', name: d[0], val: d[4]})}
]
  , ParserStart: "Definition"
}
if (typeof module !== 'undefined'&& typeof module.exports !== 'undefined') {
   module.exports = grammar;
} else {
   window.grammar = grammar;
}
})();
