-module(erlang_project_skeleton_Playerdue).
-export().

start() ->
    register(puno, spawn(?module, ttt(), {}),
    frontend:send(jiffy("ready")).

ttt() ->
    receive
        {2,2,2,_,_,_,_,_,_} -> 
            supervisor ! {"Player 2 has won"},
            ttt().

        {2,_,_,2,_,_,2,_} ->
            supervisor ! {"Player 2 has won"},
            ttt().

        {_,2,_,_,2,_,_,2,_} ->
            supervisor ! {"Player 2 has won"},
            ttt().

        {_,_,2,_,_,2,_,_,2} -> 
            supervisor ! {"Player 2 has won"},
            ttt().

        {_,_,_,2,2,2,_,_,_} ->
            supervisor ! {"Player 2 has won"},
            ttt().

        {_,_,_,_,_,_,2,2,2} ->
            supervisor ! {"Player 2 has won"},
            ttt().

        {2,_,_,_,2,_,_,_,2} ->
            supervisor ! {"Player 2 has won"},
            ttt().

        {_,_,2,_,2,_,2,_,_} ->
            supervisor ! {"Player 2 has won"},
            ttt()
        {_,_,_,_,_,_,_,_,_} ->
            supervisor ! {"No for Player 2"},
    end.