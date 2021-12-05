%%%-------------------------------------------------------------------
%% @doc erlang_project_skeleton top level supervisor.
%% @end
%%%-------------------------------------------------------------------

-module(erlang_project_skeleton_sup).

-behaviour(supervisor).

-export([start_link/0]).

-export([init/1]).

-define(SERVER, ?MODULE).

-spec start_link() -> {ok, pid()}.

start_link() ->
    io:fwrite("[sup]:start()...~n", []),
    supervisor:start_link({local, ?SERVER}, ?MODULE, []).

%% supervisor.

init([]) ->
    io:fwrite("[sup]:init()...~n", []),
    Procs = [
        {playerone_process, {playerone_process, start, []}, permanent, 1, worker, [playerone_process]},
        {playertwo_process, {playertwo_process, start, []}, permanent, 1, worker, [playertwo_process]}
        
    ],
    {ok, {{one_for_one, 10, 10}, Procs}}.

%% internal functions

