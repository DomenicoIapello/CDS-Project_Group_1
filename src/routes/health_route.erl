-module(health_route).

-export([init/2]).

-export([allowed_methods/2]).

-export([content_types_provided/2]).

-export([known_methods/2]).

-export([health/2]).

init(Req, State) -> {cowboy_rest, Req, State}.

allowed_methods(Req, State) ->
    {[<<"GET">>], Req, State}.

content_types_provided(Req, State) ->
    {[{{<<"application">>, <<"json">>, []}, health}],
     Req,
     State}.

health(Req, State) ->
    Message = {[{health, <<"ok">>}]},
    {jiffy:encode(Message), Req, State}.

known_methods(Req, State) ->
    Result = [<<"GET">>],
    {Result, Req, State}.
