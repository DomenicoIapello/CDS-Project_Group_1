%%%-------------------------------------------------------------------
%% @doc erlang_project_skeleton public API
%% @end
%%%-------------------------------------------------------------------

-module(erlang_project_skeleton_app).

-behaviour(application).

-export([start/2, stop/1]).

start(_StartType, _StartArgs) ->
    Dispatch = cowboy_router:compile([{'_',
                                       [{"/health", health_route, []}]}]),
    {ok, _} = cowboy:start_clear(http,
                                 [{port, 8080}],
                                 #{env => #{dispatch => Dispatch},
                                   middlewares =>
                                       [cowboy_router,
                                        ca_cowboy_middleware,
                                        cowboy_handler]}),
    erlang_project_skeleton_sup:start_link().

stop(_State) -> ok = cowboy:stop_listener(http).

%% internal functions

