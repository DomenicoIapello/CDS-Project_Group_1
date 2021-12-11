%%%-------------------------------------------------------------------
%% @doc Player One Process/Worker
%%
%%
%% @end
%%%-------------------------------------------------------------------
-module(playerone_process).

-behaviour(gen_server). % https://www.erlang.org/doc/man/gen_server.html

-export([start/0, get/1, post/2]).
-export([init/1, handle_cast/2, handle_call/3, terminate/2]).
-export([ttt/0]).

%
% wrappers:
%
start() ->
    % this function spawns and links to a new process, a gen_server.
    io:fwrite("[playerone_process.erl] Spawning Player One...(pid: ~p).~n", [self()]),
    gen_server:start_link({local, ?MODULE},
                          ?MODULE,
                          [],
                          []).

get(PlayerOne) -> 
    % ? get 
    io:fwrite("[playerone_process.erl]:get()...~n", []),
    gen_server:call(?MODULE, {get, PlayerOne}).

post(PlayerOne, Move) ->
    % post: player one can make a move?
    io:fwrite("[playerone_process.erl] ~p is about to post: ~p.~n", [PlayerOne, Move]),
    gen_server:call(?MODULE, {post, PlayerOne, Move}).

init([]) ->
    % init() is called when a connection is made to the server
    io:fwrite("[playerone_process.erl] Grid initialized.~n", []),
    Grid = "_________",
    {ok, Grid}.

handle_call({post, PlayerOne, Move}, {Pid, Tag}, Grid) ->
    % handle_call is invoked in response to gen_server:call
    io:fwrite("[playerone_process.erl] internal state of the gen_server process (current ttt grid): ~p.~n", [Grid]),
    io:fwrite("[playerone_process.erl] handle call from pid=~p, tag=~p.~n", [Pid, Tag]),
    io:fwrite("[playerone_process.erl] handling call for ~p.~n", [PlayerOne]),
    NewGrid = Move,
    Response = {<<"PlayerOne has moved">>, 201},
    {reply, Response, NewGrid};  % Response is the data that can be used/read. NewGrid is the NewState: https://www.erlang.org/doc/man/gen_server.html#Module:handle_call-3

handle_call({get, PlayerOne}, _From, Grid) ->
    io:fwrite("[playerone_process.erl]:handle_call({get, PlayerOne}): ...~n", []),
    Response = PlayerOne,
    {reply, Response, Grid};

handle_call({get}, _From, Grid) ->
    Response = <<"Nothing is going here">>,
    {reply, Response, Grid}.

handle_cast(_Message, Grid) -> 
    io:fwrite("[playerone_process.erl] handle cast - default no reply.~n", []),
    {noreply, Grid}.  % required by gen_server

%
% Clean Up
%
terminate(_Reason, _Grid) ->
    io:fwrite("[playerone_process.erl] terminate.~n", []),
    ok.


%
% Game Logic: process for player one
%

% idea to start/spawn ttt:
% start() ->
%    register(puno, spawn(?module, ttt(), {})),
%    supervisor ! {"ready"}.

ttt() ->
    receive
        {1,1,1,_} -> 
            supervisor ! {"Player 1 has won"},
            ttt();

        {1,_,_,1,_,_,1,_} ->
            supervisor ! {"Player 1 has won"},
            ttt();

        {_,1,_,_,1,_,_,1,_} ->
            supervisor ! {"Player 1 has won"},
            ttt();

        {_,_,1,_,_,1,_,_,1} -> 
            supervisor ! {"Player 1 has won"},
            ttt();

        {_,_,_,1,1,1,_,_,_} ->
            supervisor ! {"Player 1 has won"},
            ttt();

        {_,_,_,_,_,_,1,1,1} ->
            supervisor ! {"Player 1 has won"},
            ttt();

        {1,_,_,_,1,_,_,_,1} ->
            supervisor ! {"Player 1 has won"},
            ttt();

        {_,_,1,_,1,_,1,_,_} ->
            supervisor ! {"Player 1 has won"},
            ttt();
        {_,_,_,_,_,_,_,_,_} ->
            supervisor ! {"No for Player 1"}
    end.
