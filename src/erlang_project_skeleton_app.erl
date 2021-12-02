%%%-------------------------------------------------------------------
%% @doc erlang_project_skeleton public API
%% @end
%%%-------------------------------------------------------------------

-module(erlang_project_skeleton_app).

-behaviour(application).

-export([start/2, stop/1]).

start(_StartType, _StartArgs) ->
    % Routing
    Dispatch = cowboy_router:compile([{'_',
                                       [{"/health", health_route, []},
                                       {"/playerone", playerone_route, []}]}]),
    
    io:fwrite("[app]Dispatch done.~n", []),
    % Listener
    {ok, _} = cowboy:start_clear(http,
                                 [{port, 8080}],
                                 #{env => #{dispatch => Dispatch},
                                   middlewares =>
                                       [cowboy_router,
                                        ca_cowboy_middleware,
                                        cowboy_handler]}),

    io:fwrite("[app]Cowboy Start Clear done.~n", []),


    player_one:post(<<"PlayerOne">>, <<"0000X0000">>),
    io:fwrite("[app]PlayerOne moved.~n", []),

    erlang_project_skeleton_sup:start_link(),
    io:fwrite("[app]Superviser start link done.~n", []).

stop(_State) -> ok = cowboy:stop_listener(http).

%% internal functions
