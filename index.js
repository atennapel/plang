var _repl = require('./repl');

function getOutput(s, cb) {
  _repl.eval(s, cb);
}

var hist = [], index = -1;
var input = document.getElementById('input');
var content = document.getElementById('content');
var res = {};
(window.onresize = function() {
	content.style.height = window.innerHeight;
})();
addResult("REPL");
input.focus();
input.onkeydown = function(keyEvent) {
	var val = input.value;
	var txt = (val || '').trim();
	if(keyEvent.keyCode === 13) {
		keyEvent.preventDefault();
		if(keyEvent.ctrlKey) {	
			input.value += '\n';
		} else if(txt) {
			hist.push(val);
			index = hist.length;
			input.value = '';
			var div = document.createElement('div');
			div.innerHTML = val;
			div.className = 'line input';
			content.insertBefore(div, input);
			getOutput(txt, addResult);
		}
	} else if(keyEvent.keyCode === 38 && keyEvent.ctrlKey && index > 0) {
		keyEvent.preventDefault();
		input.value = hist[--index];
	} else if(keyEvent.keyCode === 40 && keyEvent.ctrlKey && index < hist.length-1) {
		keyEvent.preventDefault();
		input.value = hist[++index];
	} else if(keyEvent.keyCode === 40 && keyEvent.ctrlKey && index >= hist.length-1) {
		index = hist.length;
		input.value = '';
	}
}

function addResult(msg, err) {
	var divout = document.createElement('div');
	divout.className = 'line output';
	if(err) divout.className += ' error';
	divout.innerHTML = '' + msg;
	content.insertBefore(divout, input);
	input.focus();
	content.scrollTop = content.scrollHeight;
	return divout;
}

