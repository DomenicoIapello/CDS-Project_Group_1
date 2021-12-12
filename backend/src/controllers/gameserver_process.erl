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
-export([analyze_post_request/1, get_intial_grid/0]).
% Game Logic
-export([send/1, distribute/1, bmulticast/1, sendToFrontEnd/1, datareceive/0]).
-export([check_stalemate/1]).
% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, terminate/2]).


%%% ==========================================================================
%%% API
%%% ==========================================================================

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
    io:fwrite("[gameserver_process.erl] Start analyzing DecodedData from POST request...~n", []),
    gen_server:call(?MODULE, {DecodedData}).

%% -------------------------------------------------------------------------
%% @doc
%% Provide answer to get 
%%
%% @spec
%% @end
%% -------------------------------------------------------------------------
get_intial_grid() ->
    gen_server:call(?MODULE, {}).

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

send(data) ->
    genserver:post(data).

distribute(Current_Grid) ->
    Player_answer = bmulticast(Current_Grid),
    % io:format("simulate distribute stuff: ~p.~n", [Player_answer]),
    Player_answer.

bmulticast(_Current_Grid) ->
    % io:format("simulate bmulticast stuff...~p.~n", [Current_Grid]),
    Player_answer = 0,
    Player_answer.

sendToFrontEnd(_SendData) ->
    io:format("We want to send to frondend: ").

datareceive() ->
    receive
        {"Player 1 has won"} ->
            SendData = ("Winner: Player 1"),
            sendToFrontEnd(SendData),
            datareceive();

        {"No for Player 1"} ->
            SendData = ("Next step for Player 2"),
            sendToFrontEnd(SendData),
            datareceive();

        {"Player 2 has won"} ->
            SendData = ("Winner: Player 2"),
            sendToFrontEnd(SendData),
            datareceive();

        {"No for Player 2"} ->
            SendData = ("Next step for Player 1"),
            sendToFrontEnd(SendData),
            datareceive()
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
    io:fwrite("[gameserver_process.erl] Spawning Game Server...(pid: ~p).~n", [self()]),
    io:fwrite("[gameserver_process.erl] (with Name=~p, Limit=~p, SupID=~p, MFA=~p).~n", [Name, Limit, Sup_PID, MFA]),
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
%% init() is called when a connection is made to the server.
%% Technically speaking, the "Grid" represent the internal State of the server.
%%
%% @spec
%% @end
%% -------------------------------------------------------------------------
init([]) ->
    io:fwrite("[gameserver_process.erl] Grid initialized.~n", []),
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
    io:fwrite("[gameserver_process.erl]:handle_call multicasts and check for stalemate...~n", []),
    NewGrid = DecodedData,  % assume/set the received grid (DecodedData) as the new state of the server (_Grid)

    % multicast to player processes (to check if anyone has won)
    % PlayerOne_status = distribute(DecodedData), % call something, and return 0 (not win) or 1 (win)
    {PlayerOne_status, _} = playerone_process:analyze_grid(DecodedData), % call something, and return 0 (not win) or 1 (win)
    PlayerTwo_status = distribute(DecodedData), % call something, and return 0 (not win) or 1 (win)
    % check if grid has any empty cells or not (stalemate)
    Stalemate_status = check_stalemate(DecodedData),

    % waiting time to "ensure" players' answer are received
    % timer:sleep(100)

    % compile the answer from player processes and stalemate status
    Response = [PlayerOne_status, PlayerTwo_status, Stalemate_status],
    io:fwrite("[gameserver_process.erl]:response = ~p. (With 1 or 0, true/false, for {player one wins, player two wins, empty cells available}).~n", [Response]),
    {reply, {Response, 201}, NewGrid};  

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
    io:fwrite("[gameserver_process.erl] terminate.~n", []),
    ok.
