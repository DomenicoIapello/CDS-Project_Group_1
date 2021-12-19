%%%-------------------------------------------------------------------
%% @doc Player Two Process/Worker
%%
%%
%% @end
%%%-------------------------------------------------------------------
-module(playertwo_process).

-behaviour(gen_server). % https://www.erlang.org/doc/man/gen_server.html

-export([start/0, get/1, analyze_grid/1]).
-export([init/1, handle_cast/2, handle_call/3, terminate/2]).
-export([ttt/1]).


%% -------------------------------------------------------------------------
%% @doc
%% Start the Game Server
%%
%% @spec start() -> {ok,Pid} | ignore | {error,Error}
%% @end
%% -------------------------------------------------------------------------
start() ->
    % this function spawns and links to a new process, a gen_server.
    io:fwrite("[playertwo_process.erl (pid=~p)] Spawning Player Two...~n", [self()]),
    gen_server:start_link({local, ?MODULE},
                          ?MODULE,
                          [],
                          []).
%%% ==========================================================================
%%% API
%%% ==========================================================================

%% -------------------------------------------------------------------------
%% @doc
%% The Game Server Process forwards the current grid (decoded data received from frontend)
%% to Player Two Process to analyze if Player Two wins (or not).
%%
%% @spec
%% @end
%% -------------------------------------------------------------------------
analyze_grid(DecodedData) ->
    % post: player one can make a move?
    io:fwrite("[playertwo_process.erl (pid=~p)] game server sent current grid.~n", [self()]),
    gen_server:call(?MODULE, {post, DecodedData}).

%% -------------------------------------------------------------------------
%% @doc
%% Provide answer to get 
%%
%% @spec
%% @end
%% -------------------------------------------------------------------------
get(PlayerTwo) -> 
    % ? get 
    io:fwrite("[playertwo_process.erl]:get()...~n", []),
    gen_server:call(?MODULE, {get, PlayerTwo}).

%%% ==========================================================================
%%% Game Logic for the Player Two
%%% 
%%% these logic are used by the handlers.
%%% NO LOGIC should be directly implemented into the handlers.
%%% ==========================================================================

ttt(DecodedData) ->
    io:fwrite("[playertwo_process (pid=~p)] pattern matching on grid...~n", [self()]),
    case DecodedData of
        [2,2,2,_,_,_,_,_,_] -> 
            io:fwrite("[playertwo_process] Player 2 has won.~n", []),
            1;
        [2,_,_,2,_,_,2,_,_] ->
            io:fwrite("[playertwo_process] Player 2 has won.~n", []),
            1;
        [_,2,_,_,2,_,_,2,_] ->
            io:fwrite("[playertwo_process] Player 2 has won.~n", []),
            1;
        [_,_,2,_,_,2,_,_,2] ->
            io:fwrite("[playertwo_process] Player 2 has won.~n", []),
            1;
        [_,_,_,2,2,2,_,_,_] ->
            io:fwrite("[playertwo_process] Player 2 has won.~n", []),
            1;
        [_,_,_,_,_,_,2,2,2] ->
            io:fwrite("[playertwo_process] Player 2 has won.~n", []),
            1;
        [2,_,_,_,2,_,_,_,2] ->
            io:fwrite("[playertwo_process] Player 2 has won.~n", []),
            1;
        [_,_,2,_,2,_,2,_,_] ->
            io:fwrite("[playertwo_process] Player 2 has won.~n", []),
            1;
        [_,_,_,_,_,_,_,_,_] ->
            io:fwrite("[playertwo_process] Player 2 has NOT won.~n", []),
            0
    end.



%%% ==========================================================================
%%% gen_server callbacks
%%% ==========================================================================
init([]) ->
    % init() is called when a connection is made to the server
    io:fwrite("[playertwo_process.erl (pid=~p)] Grid initialized.~n", [self()]),
    Grid = [0,0,0,0,0,0,0,0,0],
    {ok, Grid}.


%% -------------------------------------------------------------------------
%% @doc
%% Handle call messages
%%
%% @spec
%% @end
%% -------------------------------------------------------------------------
handle_call({post, DecodedData}, {Pid, _Tag}, _Grid) ->
    % handle_call is invoked in response to gen_server:call
    io:fwrite("[playertwo_process.erl (pid=~p)] handle call: from pid=~p.~n", [self(), Pid]),
    NewGrid = DecodedData,
    Response = ttt(DecodedData),
    {reply, {Response, 201}, NewGrid};  % Response is the data that can be used/read. NewGrid is the NewState: https://www.erlang.org/doc/man/gen_server.html#Module:handle_call-3

handle_call({get, PlayerTwo}, _From, Grid) ->
    io:fwrite("[playertwo_process.erl]:handle_call({get, PlayerTwo}): ...~n", []),
    Response = PlayerTwo,
    {reply, Response, Grid};

handle_call({get}, _From, Grid) ->
    Response = <<"Nothing is going here">>,
    {reply, Response, Grid}.


%% -------------------------------------------------------------------------
%% @doc
%% Handle cast messages: implemented because it's required by gen_server 
%%
%%
%% @spec
%% @end
%% -------------------------------------------------------------------------
handle_cast(_Message, Grid) -> 
    io:fwrite("[playertwo_process.erl] handle cast - default no reply.~n", []),
    {noreply, Grid}.  % required by gen_server

%% -------------------------------------------------------------------------
%% @doc
%% Clean up remaining when the gen_server is terminated.
%%
%% @spec
%% @end
%% -------------------------------------------------------------------------
terminate(_Reason, _Grid) ->
    io:fwrite("[playertwo_process.erl] terminate.~n", []),
    ok.
