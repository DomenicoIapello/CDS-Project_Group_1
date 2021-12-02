-module(erlang_project_skeleton_Playeruno).
-export().

start() ->
    register(puno, spawn(?module, ttt(), {}),
    frontend:send(jiffy("ready")).

ttt() ->
    receive
        {1,1,1,_} -> 
            frontend:send(jiffy:encode("Player 1 has won")),
            ttt().

        {1,_,_,1,_,_,1,_} ->
            frontend:send(jiffy:encode("Player 1 has won")),
            ttt().

        {_,1,_,_,1,_,_,1,_} ->
            frontend:send(jiffy:encode("Player 1 has won")),
            ttt().

        {_,_,1,_,_,1,_,_,1} -> 
            frontend:send(jiffy:encode("Player 1 has won")),
            ttt().

        {_,_,_,1,1,1,_,_,_} ->
            frontend:send(jiffy:encode("Player 1 has won")),
            ttt().

        {_,_,_,_,_,_,1,1,1} ->
            frontend:send(jiffy:encode("Player 1 has won")),
            ttt().

        {1,_,_,_,1,_,_,_,1} ->
            frontend:send(jiffy:encode("Player 1 has won")),
            ttt().

        {_,_,1,_,1,_,1,_,_} ->
            frontend:send(jiffy:encode("Player 1 has won")),
            ttt()
        {basecase} ->
            frontend:send(jiffy:encode("No"))
    end.