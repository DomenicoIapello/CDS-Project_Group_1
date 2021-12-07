document.addEventListener('DOMContentLoaded', function(){
    document.getElementById("reset").addEventListener('click', resetGame);
    document.querySelector('#board').addEventListener('click', markCell);
    
    var current = 0; //marks the current player 0=player1, 1=player2
    var players = ['1', '2']; //array of players
    var reset = ['0'];
    var cells = 0; //variable to check if another cell is empty
    const data = [0,0,0,0,0,0,0,0,0]; //data of cells 0=not chosen, 1=chosen by player1, 2=chosen by player2
    var datainjson = "{uba}";
    
    function resetGame(c) {
        data = [0,0,0,0,0,0,0,0,0];
        document.querySelector('#cell1').innerText = " ";
        document.querySelector("#cell1").disabled = false;
        cells = 0;
        current = 0;
        document.querySelector('#demo').innerText = data;
    }

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
        let textdata = data.toString();
        datainjson = JSON.stringify(textdata);
        //don't know why but the same code doesn't work here but works online: https://reqbin.com/req/javascript/uzf79ewc/javascript-post-request-example
        //posting on this bin http://ptsv2.com/t/vh7me-1638884442
        sendData(datainjson);
        document.querySelector('#backenddata').innerText = datainjson;
    }

    if(data[0] == 1 && data[4] == 1 && data[8] == 1){
        document.querySelector('#Player').innerText = p1win();
    }

    //put the data array in a JSON format
    //var datainjson = JSON.stringify(data);
    
    /*create a new HTTP request which can be fetched by the cowboy REST service
    (source:https://stackoverflow.com/questions/36975619/how-to-call-a-rest-web-service-api-from-javascript)*/
    function sendData(x) {
        var url = "http://ptsv2.com/t/vh7me-1638884442/post";

        var xhr = new XMLHttpRequest();
        xhr.open("POST", url);

        xhr.setRequestHeader("Accept", "application/json");
        xhr.setRequestHeader("Content-Type", "application/json");

        xhr.onreadystatechange = function () {
            if (xhr.readyState === 4) {
            console.log(xhr.status);
            console.log(xhr.responseText);
        }}
        xhr.send(x);
    }

    /*function p1win(){
        return 'You\'re the champion! Congratulations!';
    }

    function test(){
        const axios = require('axios');
        return "salut";
    }*/
    

})