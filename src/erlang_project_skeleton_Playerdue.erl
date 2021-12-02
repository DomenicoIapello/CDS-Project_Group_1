-module(erlang_project_skeleton_Playerdue).
-export().

start() ->
    register(puno, spawn(?module, ttt(), {}),
    frontend:send(jiffy("ready")).

ttt() ->
    receive
        {2,2,2,_} -> 
            frontend:send(jiffy:encode("Player 2 has won")),
            ttt().

        {2,_,_,2,_,_,2,_} ->
            frontend:send(jiffy:encode("Player 2 has won")),
            ttt().

        {_,2,_,_,2,_,_,2,_} ->
            frontend:send(jiffy:encode("Player 2 has won")),
            ttt().

        {_,_,2,_,_,2,_,_,2} -> 
            frontend:send(jiffy:encode("Player 2 has won")),
            ttt().

        {_,_,_,2,2,2,_,_,_} ->
            frontend:send(jiffy:encode("Player 2 has won")),
            ttt().

        {_,_,_,_,_,_,2,2,2} ->
            frontend:send(jiffy:encode("Player 2 has won")),
            ttt().

        {2,_,_,_,2,_,_,_,2} ->
            frontend:send(jiffy:encode("Player 2 has won")),
            ttt().

        {_,_,2,_,2,_,2,_,_} ->
            frontend:send(jiffy:encode("Player 2 has won")),
            ttt()
        {basecase}
            frontend:send(jiffy:encode("Player 2 has won"))
    end.