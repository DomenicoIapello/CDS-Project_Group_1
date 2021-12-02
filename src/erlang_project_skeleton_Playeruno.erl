-module(erlang_project_skeleton_Playeruno).
-export().

start() ->
    register(puno, spawn(?module, ttt(), {}),
    frontend:send(jiffy("ready")).

ttt() ->
    receive
        {b,b,b,_} -> 
            frontend:send(jiffy:encode("Player 1 has won")),
            ttt().

        {b,_,_,b,_,_,b,_} ->
            frontend:send(jiffy:encode("Player 1 has won")),
            ttt().

        {_,b,_,_,b,_,_,b,_} ->
            frontend:send(jiffy:encode("Player 1 has won")),
            ttt().

        {_,_,b,_,_,b,_,_,b} -> 
            frontend:send(jiffy:encode("Player 1 has won")),
            ttt().

        {_,_,_,b,b,b,_,_,_} ->
            frontend:send(jiffy:encode("Player 1 has won")),
            ttt().

        {_,_,_,_,_,_,b,b,b} ->
            frontend:send(jiffy:encode("Player 1 has won")),
            ttt().

        {b,_,_,_,b,_,_,_,b} ->
            frontend:send(jiffy:encode("Player 1 has won")),
            ttt().

        {_,_,b,_,b,_,b,_,_} ->
            frontend:send(jiffy:encode("Player 1 has won")),
            ttt()
    end.