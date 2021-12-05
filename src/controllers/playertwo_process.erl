-module(playertwo_process).

-behaviour(gen_server). % https://www.erlang.org/doc/man/gen_server.html

-export([start/0, get/1, post/2]).

-export([init/1, handle_cast/2, handle_call/3, terminate/2]).

%
% wrappers:
%
start() ->
    % this function spawns and links to a new process, a gen_server.
    io:fwrite("[playertwo_process.erl] spawn process.~n", []),
    gen_server:start_link({local, ?MODULE},
                          ?MODULE,
                          [],
                          []).

get(PlayerTwo) -> 
    % ? get 
    io:fwrite("[playertwo_process.erl]:get()...~n", []),
    gen_server:call(?MODULE, {get, PlayerTwo}).

post(PlayerTwo, Move) ->
    % post: player one can make a move?
    io:fwrite("[playertwo_process.erl] ~p is about to post: ~p.~n", [PlayerTwo, Move]),
    gen_server:call(?MODULE, {post, PlayerTwo, Move}).

init([]) ->
    % init() is called when a connection is made to the server
    io:fwrite("[playertwo_process.erl] Grid initialized.~n", []),
    Grid = "_________",
    {ok, Grid}.

handle_call({post, PlayerTwo, Move}, {Pid, Tag}, Grid) ->
    % handle_call is invoked in response to gen_server:call
    io:fwrite("[playertwo_process.erl] internal state of the gen_server process: ~p.~n", [Grid]),
    io:fwrite("[playertwo_process.erl] handle call from pid=~p, tag=~p.~n", [Pid, Tag]),
    io:fwrite("[playertwo_process.erl] handling call for ~p.~n", [PlayerTwo]),
    NewGrid = Move,
    Response = {<<"PlayerTwo has moved">>, 201},
    {reply, Response, NewGrid};  % return value.

handle_call({get, PlayerTwo}, _From, Grid) ->
    io:fwrite("[playertwo_process.erl]:handle_call({get, PlayerTwo}): ...~n", []),
    Response = PlayerTwo,
    {reply, Response, Grid};

handle_call({get}, _From, Grid) ->
    Response = <<"Nothing is going here">>,
    {reply, Response, Grid}.

handle_cast(_Message, Grid) -> 
    io:fwrite("[playertwo_process.erl] handle cast - default no reply.~n", []),
    {noreply, Grid}.  % required by gen_server

%
% Clean Up
%
terminate(_Reason, _Grid) ->
    io:fwrite("[playertwo_process.erl] terminate.~n", []),
    ok.