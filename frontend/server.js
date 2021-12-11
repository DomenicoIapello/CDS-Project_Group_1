const http = require('http')
const fs = require('fs')
const axios = require('axios');

// Variables
const port = 3000;
var backendURL = 'http://localhost:8080/gameserver/';
var currentGrid = null;

// Functions
function sendFile(filename, contentType, res) {
    fs.readFile(filename, function(error, data) {
        if (error) {
            console.log(error);
            res.writeHead(404, { 'Content-Type': 'text/plain' });
            res.write('Something does not work well');
        } else {
            res.writeHead(200, { 'Content-Type': contentType });
            res.write(data);
        }
        res.end();
    });
}

function backendGetJSON(backendURL){
    console.log("Trying to get some GET answer...");
    axios.get(backendURL).then(resp => {
        console.log("My data is: ", resp.data);
    });
}

function backendPostJson(backendURL) {
    let payload = {"current_grid": [0,0,0,1,1,1,2,2,2]};

    let res = axios.post(backendURL, payload);

    let data = res.data;
    console.log(data);
}

// SERVER
const server = http.createServer(function(req, res) {
    if (req.url === "/ttt.js") {
        console.log('/ttt.js requested!')
        sendFile('ttt.js', 'text/javascript', res);
    } else if (req.url === "/") {
        sendFile('frontend.html', 'text/html', res);
    } else {
        res.statusCode = 404;
        res.end();
    }
})

server.listen(port, function(error) {
    if (error) {
        console.log('Something went wrong' + error)
    } else {
        backendGetJSON(backendURL);
        backendPostJson(backendURL);
        console.log('Everything is working fine on port ' + port)
    }
})
