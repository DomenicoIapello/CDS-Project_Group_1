%%%-------------------------------------------------------------------
%% @doc Game Server Process/Worker
%%
%% Responsible to Multicast
%%
%% @end
%%%-------------------------------------------------------------------
-module(gameserver_process).

-behaviour(gen_server). % https://www.erlang.org/doc/man/gen_server.html

-export([start/0, get/1, post/2]).
-export([init/1, handle_cast/2, handle_call/3, terminate/2]).
-export([send/1, distribute/1, bmulticast/1, sendToFrontEnd/1, datareceive/0]).

%
% wrappers:
%
start() ->
    % this function spawns and links to a new process, a gen_server.
    io:fwrite("[gameserver_process.erl] spawn process.~n", []),
    gen_server:start_link({local, ?MODULE},
                          ?MODULE,
                          [],
                          []).

get(GameServer) -> 
    % ? get 
    io:fwrite("[gameserver_process.erl]:get()...~n", []),
    gen_server:call(?MODULE, {get, GameServer}).

post(GameServer, Move) ->
    % post: player one can make a move?
    io:fwrite("[gameserver_process.erl] ~p is about to post: ~p.~n", [GameServer, Move]),
    gen_server:call(?MODULE, {post, GameServer, Move}).

init([]) ->
    % init() is called when a connection is made to the server
    io:fwrite("[gameserver_process.erl] Grid initialized.~n", []),
    Grid = "_________",
    {ok, Grid}.

handle_call({post, GameServer, Move}, {Pid, Tag}, Grid) ->
    % handle_call is invoked in response to gen_server:call
    io:fwrite("[gameserver_process.erl] internal state of the gen_server process: ~p.~n", [Grid]),
    io:fwrite("[gameserver_process.erl] handle call from pid=~p, tag=~p.~n", [Pid, Tag]),
    io:fwrite("[gameserver_process.erl] handling call for ~p.~n", [GameServer]),
    NewGrid = Move,
    Response = {<<"GameServer has moved">>, 201},
    {reply, Response, NewGrid};  % return value.

handle_call({get, GameServer}, _From, Grid) ->
    io:fwrite("[gameserver_process.erl]:handle_call({get, GameServer}): ...~n", []),
    Response = GameServer,
    {reply, Response, Grid};

handle_call({get}, _From, Grid) ->
    Response = <<"Nothing is going here">>,
    {reply, Response, Grid}.

handle_cast(_Message, Grid) -> 
    io:fwrite("[gameserver_process.erl] handle cast - default no reply.~n", []),
    {noreply, Grid}.  % required by gen_server

%
% Clean Up
%
terminate(_Reason, _Grid) ->
    io:fwrite("[gameserver_process.erl] terminate.~n", []),
    ok.

%
% Game Logic: process for player one
%
send(data) ->
    genserver:post(data).

distribute(data) ->
    datastream = jiffy:decode(data),
    bmulticast(datastream).

bmulticast(datastream) ->
    io:format(datastream).

sendToFrontEnd(sendData) ->
    io:format("We want to send to frondend: ").

datareceive() ->
    receive
        {"Player 1 has won"} ->
            sendData = jiffy:encode("Winner: Player 1"),
            sendToFrontEnd(sendData),
            datareceive();

        {"No for Player 1"} ->
            sendData = jiffy:encode("Next step for Player 2"),
            sendToFrontEnd(sendData),
            datareceive();

        {"Player 2 has won"} ->
            sendData = jiffy:encode("Winner: Player 2"),
            sendToFrontEnd(sendData),
            datareceive();

        {"No for Player 2"} ->
            sendData = jiffy:encode("Next step for Player 1"),
            sendToFrontEnd(sendData),
            datareceive()
    end.