%%%-------------------------------------------------------------------
%% @doc ttt (tic-tac-toe) top level supervisor.
%%
%% This supervisor will create the supervision tree of our tic-tac-toe (ttt) application,
%% and we can have, as such, a hierarchical process structure ("a nice way to 
%% structure a fault-tolerant application").
%%
%% This supervisor is responsible for starting, stopping, and monitoring:
%% - process for game server
%% - process for player one
%% - process for player two
%%
%% 
%% Supervisor's official doc: https://www.erlang.org/doc/man/supervisor.html
%%
%% @end
%%%-------------------------------------------------------------------

-module(ttt_sup).

-behaviour(supervisor).

-export([start_link/3]).

-export([init/1]).

-define(SERVER, ?MODULE).

-spec start_link(_Name, _Limit, _MFA) -> {ok, pid()}.

start_link(Name, Limit, MFA) ->
    io:fwrite("[sup] Starting top supervisor of our app...(pid: ~p)~n", [self()]),
    supervisor:start_link({local, ?SERVER}, ?MODULE, [Name, Limit, MFA]).

%% supervisor.
%{M,F,A} := Module name, Function, Arguments.
init([Name, Limit, MFA]) ->
    io:fwrite("[sup] creating the supervision tree...~n", []),
    
    Procs = [
        {
            gameserver_process, 
            {
                gameserver_process,         % M
                start,                      % F
                [Name, Limit, self(), MFA]  % A
            },
            permanent,
            1,                              % shutdown time
            worker,
            [gameserver_process]
        },
        {playerone_process, {playerone_process, start, []}, permanent, 1, worker, [playerone_process]},
        {playertwo_process, {playertwo_process, start, []}, permanent, 1, worker, [playertwo_process]}                
    ],

    {
        ok, 
        {
            {
                one_for_one,  % ttt_sup restarts only the process that crash (one by one).
                10,           % MaxRestart
                10            % MaxTime
            }, 
            Procs
        }
    }.

%% internal functions

