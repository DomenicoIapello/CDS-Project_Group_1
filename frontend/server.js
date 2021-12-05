let http = require('http');
let fs = require('fs');

let handleRequest = (request, response) => {
    response.writeHead(200, {
        'Content-Type': 'text/html'
    });
    const myFrontend = fs.readFile('./frontend.html', null, function (error, data) {
        if (error) {
            response.writeHead(404);
            respone.write('Whoops! File not found!');
        } else {
            response.write(data);
        }
        response.end();
    });
};

http.createServer(handleRequest).listen(3000);


//
// TODO: how to add an event listener on frontend.html's '#board' 
//

    // // put the data array in a JSON format
    // var datainjson = JSON.stringify(data);

    // document.querySelector('#board').addEventListener('click', markCell);
    // var current = 0; //marks the current player 0=player1, 1=player2
    // var players = ['1', '2']; //array of players
    // var cells = 0; //variable to check if another cell is empty
    // const data = [0,0,0,0,0,0,0,0,0]; //data of cells 0=not chosen, 1=chosen by player1, 2=chosen by player2

//
// Functions
//
function markCell(c) {
    var cell = c.target; //get the clicked cell
    var id = c.target.id; //get the id of the clicked cell

    cell.setAttribute('aria-label', players[current]); //set the label of the clicked cell on the playernumber
    document.querySelector('#' + id).innerText = players[current]; //set the text of the clicked cell on the playernumber
    cell.setAttribute('disabled', 'disabled'); //set the clicked cell on disabled so you can't click it twice
    var ider = id.substr(4); //strip the first 4 letters of the cellid to be left with just the number
    /*not pretty, but what it does is if the current player has value of 0(which means player1)
    it should append to the position of the cell (cell-1 because the array starts at 0 and not 1) the number 1.
    The same for the value of 1(which means player2) it should add at the index of the id-1 the number 2
     */
    if(current==0){
        data[ider-1] = current+1;
    } else {
        data[ider-1] = current+1;
    }

    current = 1 - current; //set the current player to the other player for the next round

    document.querySelector('#Player').innerText = 'Its the turn of Player ' + players[current]; //display whos turn it is

    cells = cells + 1; //rise the number of cells clicked

    document.querySelector('#demo').innerText = data; //show what the current data of the cells is
           
    //check if another cell is empty, if not the text is displayed
    if(cells == 9) {
        document.querySelector('#Player').innerText = 'No Player has won, no more empty cells';
    }
}

function sendData() {
    //create a new HTTP request which can be fetched by the cowboy REST service
    //(source:https://stackoverflow.com/questions/36975619/how-to-call-a-rest-web-service-api-from-javascript)
    var xhttp = new XMLHttpRequest();
    xhttp.onreadystatechange = function() {
            if (this.readyState == 4 && this.status == 200) {
                alert(this.responseText);
            }
    };
    xhttp.open("POST", "Localhost:8080", true);
    xhttp.setRequestHeader("Content-type", "application/json");
    xhttp.send(datainjson);
}
