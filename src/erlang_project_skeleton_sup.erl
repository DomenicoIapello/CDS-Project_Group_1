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
    supervisor:start_link({local, ?SERVER}, ?MODULE, []).

%% supervisor.

init([]) ->
    Procs = [],
    {ok, {{one_for_one, 10, 10}, Procs}}.

%% internal functions

