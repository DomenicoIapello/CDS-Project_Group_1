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

-export([start_link/0]).

-export([init/1]).

-define(SERVER, ?MODULE).

-spec start_link() -> {ok, pid()}.

start_link() ->
    io:fwrite("[sup] Starting top supervisor of our app...(pid: ~p)~n", [self()]),
    supervisor:start_link({local, ?SERVER}, ?MODULE, []).

%% supervisor.

init([]) ->
    io:fwrite("[sup] creating the supervision tree...~n", []),
    Procs = [
        {gameserver_process, {gameserver_process, start, []}, permanent, 1, worker, [gameserver_process]},
        {playerone_process, {playerone_process, start, []}, permanent, 1, worker, [playerone_process]},
        {playertwo_process, {playertwo_process, start, []}, permanent, 1, worker, [playertwo_process]}                
    ],
    {ok, {{one_for_one, 10, 10}, Procs}}.

%% internal functions

