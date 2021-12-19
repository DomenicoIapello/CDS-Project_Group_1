%%%-------------------------------------------------------------------
%% @doc Game Server Process/Worker
%%
%% Responsible to manage the tic-tac-toe grid (create, say when it's a stalemate),
%% and Multicast Updates (moves of each player, current grid to frontend).
%%
%% - can return an empty/initialized grid to the frontend (HTTP GET request by frontend)
%% - analyze current grid received by the frontend (HTTP POST request by frontend)
%% - multicast current grid to player processes to analyze if anyone win
%% - analyze if grid has empty cells or not (stalemate => end game without winner)
%%
%%
%% @end
%%%-------------------------------------------------------------------
-module(gameserver_process).

-behaviour(gen_server). % https://www.erlang.org/doc/man/gen_server.html

% API
-export([start/4]).
-export([init_gameserver/0, analyze_post_request/1, get_intial_grid/0]).
% Game Logic
-export([check_stalemate/1]).
% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, terminate/2]).


%%% ==========================================================================
%%% API
%%% ==========================================================================

%% -------------------------------------------------------------------------
%% @doc
%% This lets a user initialize the gameserver process.
%% and behind the hood, the init([]) function is called.
%%
%% @spec
%% @end
%% -------------------------------------------------------------------------
init_gameserver() ->
    gen_server:call({global, ?MODULE}, {userInteract, initProcess}).

%% -------------------------------------------------------------------------
%% @doc
%% The Frontend sends a POST request with current grid (i.e. DecodedData),
%% that grid contains the last player's move.
%%
%% Here, the process needs to analyze the grid and give an answer on 
%% - whether any of the player has won, and
%% - if the grid is in a stalemate or not (i.e. at least one empty cell available)
%%
%% @spec
%% @end
%% -------------------------------------------------------------------------
analyze_post_request(DecodedData) ->
    io:fwrite("[gameserver_process.erl (pid=~p)] Start analyzing DecodedData from POST request...~n", [self()]),
    gen_server:call({global, ?MODULE}, {DecodedData}).

%% -------------------------------------------------------------------------
%% @doc
%% Provide answer to get 
%%
%% @spec
%% @end
%% -------------------------------------------------------------------------
get_intial_grid() ->
    gen_server:call({global, ?MODULE}, {}).

%%% ==========================================================================
%%% Game Logic for the Game Server
%%% 
%%% these logic are used by the handlers.
%%% NO LOGIC should be directly implemented into the handlers.
%%% ==========================================================================
check_stalemate(Current_Grid) ->
    case lists:member(0, Current_Grid) of
        true -> 1;  % empty cells available
        false -> 0  % stalemate
    end.


%% -------------------------------------------------------------------------
%% @doc
%% Start the Game Server
%%
%% @spec start() -> {ok,Pid} | ignore | {error,Error}
%% @end
%% -------------------------------------------------------------------------
start(Name, Limit, Sup_PID, MFA) ->
    % this function spawns and links to a new process, a gen_server.
    io:fwrite("[gameserver_process.erl (pid=~p)] Spawning Game Server...~n", [self()]),
    io:fwrite("[gameserver_process.erl] (with Name=~p, Limit=~p, SupID=~p, MFA=~p).~n", [Name, Limit, Sup_PID, MFA]),
    gen_server:start_link({global, ?MODULE},  % ServerName
                          ?MODULE,            % Module
                          [],                 % Args
                          []).                % Options


%%% ==========================================================================
%%% gen_server callbacks
%%% ==========================================================================

%% -------------------------------------------------------------------------
%% @doc
%% Initialize the Game Server
%%
%% init() is called when a connection is made to the server.
%% Technically speaking, the "Grid" represent the internal State of the server.
%%
%% In other words (from the official doc), whenever a gen_server process is 
%% started using start/3,4, start_monitor/3,4, or start_link/3,4, this function 
%% is called by the new process to initialize.
%%
%% @spec
%% @end
%% -------------------------------------------------------------------------
init([]) ->
    io:fwrite("[gameserver_process.erl (pid=~p)] Grid initialized.~n", [self()]),
    Grid = [0,0,0,0,0,0,0,0,0],
    {ok, Grid}.

%% -------------------------------------------------------------------------
%% @doc
%% Handle call messages
%%
%% @spec
%% @end
%% -------------------------------------------------------------------------
handle_call({DecodedData}, _From, _Grid) ->
    % _Grid represent the current grid that the server knows (internal state)
    io:fwrite("[gameserver_process.erl (pid=~p)]:handle_call multicasts and check for stalemate...~n", [self()]),
    NewGrid = DecodedData,  % set the received grid (DecodedData) as the new state of the server (_Grid)

    % multicast (sequentially) to player processes (to check if anyone has won)
    {PlayerOne_status, _} = playerone_process:analyze_grid(DecodedData), 
    {PlayerTwo_status, _} = playertwo_process:analyze_grid(DecodedData),
    % check if grid has any empty cells or not (stalemate)
    Stalemate_status = check_stalemate(DecodedData),

    % compile the answer from player processes and stalemate status
    Response = [PlayerOne_status, PlayerTwo_status, Stalemate_status],
    io:fwrite("[gameserver_process.erl]:response = ~p. (With 1 or 0 as true/false, for {player one wins, player two wins, empty cells available}).~n", [Response]),
    {reply, {Response, 201}, NewGrid};  

handle_call({userInteract, initProcess}, _From, Grid) ->
    init([]),
    {reply, gridInitiated, Grid};

handle_call({msg, MyMsg}, _From, Grid) ->
    io:fwrite("[gameserver_process] handle_call says ~p.~n", [MyMsg]),
    {reply, response, Grid};

handle_call({}, _From, Grid) ->
    Response = {current_grid, Grid},
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
    io:fwrite("[gameserver_process.erl] handle cast - default no reply.~n", []),
    {noreply, Grid}.

%% -------------------------------------------------------------------------
%% @doc
%% Clean up remaining when the gen_server is terminated.
%%
%% @spec
%% @end
%% -------------------------------------------------------------------------
terminate(_Reason, _Grid) ->
    io:fwrite("[gameserver_process.erl (pid=~p)] terminate.~n", [self()]),
    ok.
