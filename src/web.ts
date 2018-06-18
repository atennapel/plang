import run from './repl';

function getOutput(s: string, cb: (output: string, err?: boolean) => void): void {
  run(s, cb);
}

var hist: any[] = [], index = -1;
var input = document.getElementById('input') as any;
var content = document.getElementById('content') as any;
var res = {};
function onresize() {
	content.style.height = window.innerHeight;
}
window.addEventListener('resize', onresize);
onresize();
addResult("REPL");
input.focus();
input.onkeydown = function(keyEvent: any) {
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

function addResult(msg: string, err?: boolean) {
	var divout = document.createElement('div');
	divout.className = 'line output';
	if(err) divout.className += ' error';
	divout.innerHTML = '' + msg;
	content.insertBefore(divout, input);
	input.focus();
	content.scrollTop = content.scrollHeight;
	return divout;
}
