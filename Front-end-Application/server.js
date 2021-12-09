var http = require('http');
var url = require('url');

var myserver = http.createServer();

myserver.on("request", function returnResults(req, res) {
    res.setHeader("Access-Control-Allow-Origin", "*");
    res.writeHead(200, {'Content-Type': 'application/json'});

myserver.listen(3030);})