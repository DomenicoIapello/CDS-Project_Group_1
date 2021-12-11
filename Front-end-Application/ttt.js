document.addEventListener('DOMContentLoaded', function(){
    document.querySelector('#board').addEventListener('click', markCell);
    
    var current = 0; //marks the current player 0=player1, 1=player2
    var players = ['1', '2']; //array of players
    var reset = ['0'];
    var cells = 0; //variable to check if another cell is empty
    const data = [0,0,0,0,0,0,0,0,0]; //data of cells 0=not chosen, 1=chosen by player1, 2=chosen by player2
    var datainjson = "{uba}";
    var display = "";

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



        //new code from here
        var dataPost = new XMLHttpRequest();
        var url = "http://ptsv2.com/t/vh7me-1638884442/post";
        dataPost.open("POST", url, false);
        dataPost.send(datainjson);
                var result = dataPost.responseText;
                display = "test";
                if(result == "1,0,0" || result == "1,0,1"){
                    display = "Player 1 has won";
                } else if (result == "0,1,0" || result  == "0,1,1" ){
                    display = "Player 2 has won";
                } else {
                    display = "No player has won and no more empty cells are available. It's a tie"
                }

            document.querySelector('#Player').innerText = display;

        //to here
        document.querySelector('#backenddatasend').innerText = datainjson;
        document.querySelector('#backenddatareceive').innerText = result;

    }

    if(data[0] == 1 && data[4] == 1 && data[8] == 1){
        document.querySelector('#Player').innerText = p1win();
    }
    /*create a new HTTP request which can be fetched by the cowboy REST service
    (source:https://stackoverflow.com/questions/36975619/how-to-call-a-rest-web-service-api-from-javascript)*/

}
)