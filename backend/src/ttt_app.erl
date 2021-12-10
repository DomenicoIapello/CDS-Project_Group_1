%%%-------------------------------------------------------------------
%% @doc ttt (tic-tac-toe) public API
%%
%% This module is where we start and stop the code of our tic-tac-toe (ttt) application.
%% For that, we use two callback functions start() and stop(), respectively.
%%
%% When the application starts, the start() function is called and it itselfs starts
%% the top supervisor which will create the supervision tree.
%%
%% TODO   --------- DETAIL MORE WHAT IS ACTUALLY STARTING ---------
%%
%%
%% The application will actually stop in an automatic fashion. Any necessary cleaning is
%% taking care by the stop() function which is called after the application has been stopped.
%%
%% @end
%%%-------------------------------------------------------------------

-module(ttt_app).

-behaviour(application).

-export([start/2, stop/1]).
-export([test_scenario_1/0]).

start(_StartType, _StartArgs) ->
    io:fwrite("Tic-Tac-Toe starting... (pid: ~p)~n", [self()]),
    % Compile routes to the resources:
    % takes a human readable list of routes and transforms it into a form more efficient to process.
    Dispatch = cowboy_router:compile([{'_',
                                       [{"/health", health_route, []},
                                       {"/playerone", playerone_route, []},
                                       {"/playertwo", playerone_route, []},
                                       {"/gameserver", gameserver_route, []}]}]),
    
    io:fwrite("\n[app] Cowboy compiled routes to the resources.~n", []),
    
    % Listener:
    % Listen for connections using plain TCP
    {ok, _} = cowboy:start_clear(http,
                                 [{port, 8080}],
                                 #{env => #{dispatch => Dispatch},
                                   middlewares =>
                                       [cowboy_router,
                                        ca_cowboy_middleware,
                                        cowboy_handler]}),

    io:fwrite("[app] Cowboy is listening for connections using TCP...~n", []),

    ttt_sup:start_link().
    
    % TESTING
    % test_scenario_1().

stop(_State) -> 
    io:fwrite("[app] App stopped. Cleaning anything remaining...~n", []),
    ok = cowboy:stop_listener(http).

%% internal functions
test_scenario_1() ->
    Stalemate_status = gameserver_process:stalemate(),
    io:fwrite("[app] the stalemate status is: ~p.~n", [Stalemate_status]).
