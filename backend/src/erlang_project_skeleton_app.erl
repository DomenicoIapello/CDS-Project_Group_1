%%%-------------------------------------------------------------------
%% @doc erlang_project_skeleton public API
%% @end
%%%-------------------------------------------------------------------

-module(erlang_project_skeleton_app).

-behaviour(application).

-export([start/2, stop/1]).

start(_StartType, _StartArgs) ->
    % Routing: maps URI paths onto the module names that will handle the individual requests
    % playerone_process:start(),  % TODO: supervisor should probably start it?
    % playertwo_process:start(),  % TODO: supervisor should probably start it?
    Dispatch = cowboy_router:compile([{'_',
                                       [{"/health", health_route, []},
                                       {"/playerone", playerone_route, []},
                                       {"/playertwo", playerone_route, []}]}]),
    
    io:fwrite("[app]Dispatch done.~n", []),
    
    % Listener: Starts the web server:
    {ok, _} = cowboy:start_clear(http,
                                 [{port, 8080}],
                                 #{env => #{dispatch => Dispatch},
                                   middlewares =>
                                       [cowboy_router,
                                        ca_cowboy_middleware,
                                        cowboy_handler]}),

    io:fwrite("[app]Cowboy Start Clear done.~n", []),


    %playerone_process:post(<<"PlayerOne">>, <<"____X____">>),
    %io:fwrite("[app]PlayerOne moved.~n", []),
    %playertwo_process:post(<<"PlayerTwo">>, <<"_O__X____">>),
    %io:fwrite("[app]PlayerTwo moved.~n", []),
    

    erlang_project_skeleton_sup:start_link().

stop(_State) -> 
    io:fwrite("[app] about to stop the app.~n", []),
    ok = cowboy:stop_listener(http).

%% internal functions
