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
    % Compile routes to the resources:
    % takes a human readable list of routes and transforms it into a form more efficient to process.
    Dispatch = cowboy_router:compile([{'_',
                                       [{"/health", health_route, []},
                                       {"/playerone", playerone_route, []},
                                       {"/playertwo", playerone_route, []},
                                       {"/gameserver", gameserver_route, []}]}]),
    
    io:fwrite("\n[app]Dispatch done.~n", []),
    
    % Listener:
    % Listen for connections using plain TCP
    {ok, _} = cowboy:start_clear(http,
                                 [{port, 8080}],
                                 #{env => #{dispatch => Dispatch},
                                   middlewares =>
                                       [cowboy_router,
                                        ca_cowboy_middleware,
                                        cowboy_handler]}),

    io:fwrite("[app]Cowboy Start Clear done.~n", []),

    % TESTING
    %test_scenario_1(),

    ttt_sup:start_link().

stop(_State) -> 
    io:fwrite("[app] about to stop the app.~n", []),
    ok = cowboy:stop_listener(http).

%% internal functions
test_scenario_1() ->
    playerone_process:post(<<"PlayerOne">>, <<"____X____">>),
    io:fwrite("[app]PlayerOne moved.~n", []),
    playertwo_process:post(<<"PlayerTwo">>, <<"_O__X____">>),
    io:fwrite("[app]PlayerTwo moved.~n", []).
