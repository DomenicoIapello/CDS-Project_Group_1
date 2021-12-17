%%%-------------------------------------------------------------------
%% @doc tttrel public API
%% @end
%%%-------------------------------------------------------------------

-module(tttrel_app).

-behaviour(application).

-export([start/2, stop/1]).

start(_StartType, _StartArgs) ->
    tttrel_sup:start_link().

stop(_State) ->
    ok.

%% internal functions
