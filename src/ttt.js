document.addEventListener('DOMContentLoaded', function(){
    document.querySelector('#board').addEventListener('click', markCell);
    var current = 0;
    var players = ['1', '2'];
    var cells = 0;
    

    function markCell(c) {
        var cell = c.target;
        var data = "";
        var id = c.target.id;
        cell.setAttribute('aria-label', players[current]);
        document.querySelector('#' + id).innerText = players[current];
        cell.setAttribute('disabled', 'disabled');
        //send message to erlang here
        current = 1 - current;
        document.querySelector('#Player').innerText = 'Its the turn of Player ' + players[current];
        cells = cells + 1;
        var i = 1;
        while(i<10) {
            var content = document.getElementById('#test');
            data += content;
            i = i+1;
        }
        //JSON.stringify(data);
        document.querySelector('#demo').innerText = data;

        if(cells == 9) {
            document.querySelector('#Player').innerText = 'No Player has won, no more empty cells';
        }
    }
})