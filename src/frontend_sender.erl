-module(frontend_sender).
-export(send/1, distribute/1).
-behaviour(gen)

send(data) ->
    genserver:post(data).

distribute(data) ->
    datastream = jiffy:decode(data),
    bmulticast(datastream).