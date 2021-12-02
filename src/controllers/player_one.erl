-module(player_one).

-behaviour(gen_server). % https://www.erlang.org/doc/man/gen_server.html

-export([start/0, get/1, gets/0, post/2]).

-export([init/1, handle_cast/2, handle_call/3, terminate/2]).


% wrappers:
start() ->
    % this function spawns and links to a new process, a gen_server.
    io:fwrite("[player_one.erl] spawn process.~n", []),
    gen_server:start_link({local, ?MODULE},
                          ?MODULE,
                          [],
                          []).

get(PlayerOne) -> 
    % ? get 
    io:fwrite("[player_one.erl] Get something.~n", []),
    gen_server:call(?MODULE, {get, PlayerOne}).

gets() -> 
    gen_server:call(?MODULE, {get}).

post(PlayerOne, Move) ->
    % post: player one can make a move?
    io:fwrite("[player_one.erl] ~p is about to post something: ~p.~n", [PlayerOne, Move]),
    gen_server:call(?MODULE, {post, PlayerOne, Move}).

% This is called when a connection is made to the server
init([]) ->
    io:fwrite("[player_one.erl] Grid initialized.~n", []),
    Grid = "",
    {ok, Grid}.

% handle_call is invoked in response to gen_server:call
handle_call({post, _PlayerOne, Move}, _From, Grid) ->
    io:fwrite("[player_one.erl] handling call.~n", []),
    NewGrid = Grid,
    Response = {Move, 201},
    {reply, Response, NewGrid}.

handle_cast(_Message, Grid) -> 
    io:fwrite("[player_one.erl] handle cast - default no reply.~n", []),
    {noreply, Grid}.  % required by gen_server

% Clean Up
terminate(_Reason, _Grid) ->
    io:fwrite("[player_one.erl] terminate.~n", []),
    ok.