%%%-------------------------------------------------------------------
%% @doc Game Server Process/Worker
%%
%% Responsible to manage the tic-tac-toe grid (create, say when it's a stalemate),
%% and Multicast Updates (moves of each player, current grid to frontend).
%%
%% - The initial grid is created by the init() call. format ="_________", 
%% one "_" per cell in the grid
%%
%% - Send message to frontend when grid is full and stalemate (not winner)
%%
%% - Send message to frontend and players when one of the two players wins (multicast).
%%
%%
%%
%% @end
%%%-------------------------------------------------------------------
-module(gameserver_process).

-behaviour(gen_server). % https://www.erlang.org/doc/man/gen_server.html

% API
-export([start/0]).
% Game Logic
-export([send/1, distribute/1, bmulticast/1, sendToFrontEnd/1, datareceive/0]).
-export([stalemate/0, get_current_grid/0]).
-export([check_stalemate/1]).
% gen_server callbacks
-export([init/1, terminate/2]).
-export([handle_call/3, handle_cast/2]).


%%% ==========================================================================
%%% API
%%% ==========================================================================
stalemate() ->
    io:fwrite("[gameserver_process.erl] the grid is full, without any winner.~n", []),
    gen_server:call(?MODULE, {stalemate}).

get_current_grid() ->
    gen_server:call(?MODULE, {get_current_grid}).

%%% ==========================================================================
%%% Game Logic for the Game Server
%%% 
%%% these logic are used by the handlers.
%%% NO LOGIC should be directly implemented into the handlers.
%%% ==========================================================================
check_stalemate(current_grid) ->
    io:format("We are about to check the grid ~p.~n", [current_grid]),
    case lists:member($0, current_grid) of
        true -> game_continue;
        false -> stalemate
    end.

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


%% -------------------------------------------------------------------------
%% @doc
%% Start the Game Server
%%
%% @spec start() -> {ok,Pid} | ignore | {error,Error}
%% @end
%% -------------------------------------------------------------------------
start() ->
    % this function spawns and links to a new process, a gen_server.
    io:fwrite("[gameserver_process.erl] Spawning Game Server...(pid: ~p).~n", [self()]),
    gen_server:start_link({local, ?MODULE},
                          ?MODULE,
                          [],
                          []).


%%% ==========================================================================
%%% gen_server callbacks
%%% ==========================================================================

%% -------------------------------------------------------------------------
%% @doc
%% Initialize the Game Server
%%
%% init() is called when a connection is made to the server
%%
%% @spec
%% @end
%% -------------------------------------------------------------------------
init([]) ->
    io:fwrite("[gameserver_process.erl] Grid initialized.~n", []),
    Grid = "_________",
    {ok, Grid}.

%% -------------------------------------------------------------------------
%% @doc
%% Handle call messages
%%
%% @spec
%% @end
%% -------------------------------------------------------------------------
handle_call({stalemate}, _From, Grid) ->
    io:fwrite("[gameserver_process.erl]:handle_call we have a stalemate. Game ends.~n", []),
    Stalemate_status = check_stalemate(Grid),
    Response = {Stalemate_status, Grid},
    {reply, Response, Grid};

handle_call({get_current_grid}, _From, Grid) ->
    Response = {current_grid, Grid},
    {reply, Response, Grid}.

%% -------------------------------------------------------------------------
%% @doc
%% Handle cast messages
%%
%% @spec
%% @end
%% -------------------------------------------------------------------------
handle_cast(_Message, Grid) -> 
    %
    % implemented because it's required by gen_server 
    %
    % Whenever a gen_server process receives a request sent using cast/2 
    % or abcast/2,3, this function is called to handle the request.
    io:fwrite("[gameserver_process.erl] handle cast - default no reply.~n", []),
    {noreply, Grid}.  % required by gen_server

%% -------------------------------------------------------------------------
%% @doc
%% Clean up when the gen_server is terminated.
%%
%% @spec
%% @end
%% -------------------------------------------------------------------------
terminate(_Reason, _Grid) ->
    io:fwrite("[gameserver_process.erl] terminate.~n", []),
    ok.
