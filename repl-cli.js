var _repl = require('./repl');
var _fs = require('fs');

var _readline = require('readline')
  .createInterface(process.stdin, process.stdout);

function _output(_str, _err) {
  console.log(_str);
}

var _clinp = process.argv[2] || null;
if(_clinp) {
  _repl.eval(_fs.readFileSync(_clinp, {encoding: 'utf8'}), _output);
  process.exit();
} else {
  console.log('REPL');
  process.stdin.setEncoding('utf8');
  function _input() {
  	_readline.question('> ', function(_i) {
      _repl.eval(_i, (s, e) => {
        _output(s, e);
        setTimeout(_input, 0);
      });
    });
  };
  _input();
}
