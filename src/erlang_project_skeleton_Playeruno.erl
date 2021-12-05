-module(erlang_project_skeleton_Playeruno).
-export().

start() ->
    register(puno, spawn(?module, ttt(), {}),
    supervisor ] {"ready"}.

ttt() ->
    receive
        {1,1,1,_} -> 
            supervisor ! {"Player 1 has won"},
            ttt().

        {1,_,_,1,_,_,1,_} ->
            supervisor ! {"Player 1 has won"},
            ttt().

        {_,1,_,_,1,_,_,1,_} ->
            supervisor ! {"Player 1 has won"},
            ttt().

        {_,_,1,_,_,1,_,_,1} -> 
            supervisor ! {"Player 1 has won"},
            ttt().

        {_,_,_,1,1,1,_,_,_} ->
            supervisor ! {"Player 1 has won"},
            ttt().

        {_,_,_,_,_,_,1,1,1} ->
            supervisor ! {"Player 1 has won"},
            ttt().

        {1,_,_,_,1,_,_,_,1} ->
            supervisor ! {"Player 1 has won"},
            ttt().

        {_,_,1,_,1,_,1,_,_} ->
            supervisor ! {"Player 1 has won"},
            ttt()
        {_,_,_,_,_,_,_,_,_} ->
            supervisor ! {"No for Player 1"},
    end.