document.addEventListener('DOMContentLoaded', function(){
    document.querySelector('#board').addEventListener('click', markCell);
    var current = 0;
    var players = ['1', '2'];
    var cells = 0;
    const data = [0,0,0,0,0,0,0,0,0];
    

    function markCell(c) {
        var cell = c.target;
        var id = c.target.id;
        cell.setAttribute('aria-label', players[current]);
        document.querySelector('#' + id).innerText = players[current];
        cell.setAttribute('disabled', 'disabled');
        var ider = id.substr(4);
        //send message to erlang here
        if(current==0){
            data[ider-1] = current+1;
        } else {
            data[ider-1] = current+1;
        }
        current = 1 - current;
        document.querySelector('#Player').innerText = 'Its the turn of Player ' + players[current];
        cells = cells + 1;
        //var i = 1;
    
        /*while(i<10) {
            var content = document.getElementById('#board');
            data += content;
            i = i+1;
        }*/
        //JSON.stringify(data);
        document.querySelector('#demo').innerText = data;
               

        if(cells == 9) {
            document.querySelector('#Player').innerText = 'No Player has won, no more empty cells';
        }
    }

        var datainjson = JSON.stringify(data);
    

    function sendData() {
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
})