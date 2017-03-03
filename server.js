var PORT = 3007;

var fs = require('fs');
var express = require('express');

var time = Date.now();
console.log('Starting up server...');
var app = express();

var sendFile = res => (err, file) => {
	if(err) res.status(500).send('Something went wrong');
	else res.send(file);
};

app.get('/font', function(req, res) {
	fs.readFile('FiraCode.ttf', sendFile(res));
});
app.get('/', function(req, res) {
	fs.readFile('index.html', {encoding: 'utf8'}, sendFile(res));
});
app.get('/bundle', function(req, res) {
	fs.readFile('bundle.js', {encoding: 'utf8'}, sendFile(res));
});
app.get('/lib', function(req, res) {
	fs.readFile('lib.js', {encoding: 'utf8'}, sendFile(res));
});
app.get('/lib/:name', function(req, res) {
	fs.readFile('lib/'+req.params.name + '.l',
		{encoding: 'utf8'}, (err, file) => {
			if(err) res.status(500).send('Library not found: ' + req.params.name);
			else res.send(file);
		});
});

app.listen(PORT);

console.log('Listening on ' + PORT +
	' (started in ' + (Date.now() - time) + 'ms)');
