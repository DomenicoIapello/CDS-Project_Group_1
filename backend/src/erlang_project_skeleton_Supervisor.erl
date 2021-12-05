-module(erlang_project_skeleton_Supervisor).
-export(send/1, distribute/1).
-behaviour(gen)

send(data) ->
    genserver:post(data).

distribute(data) ->
    datastream = jiffy:decode(data),
    bmulticast(datastream).

datareceive() ->
    receive
        {"Player 1 has won"} ->
            sendData = jiffy:encode("Winner: Player 1"),
            sendToFrontEnd(sendData),
            datareceive().

        {"No for Player 1"} ->
            sendData = jiffy:encode("Next step for Player 2"),
            sendToFrontEnd(sendData),
            datareceive().

        {"Player 2 has won"} ->
            sendData = jiffy:encode("Winner: Player 2"),
            sendToFrontEnd(sendData),
            datareceive().

        {"No for Player 2"} ->
            sendData = jiffy:encode("Next step for Player 1"),
            sendToFrontEnd(sendData),
            datareceive().
    end.