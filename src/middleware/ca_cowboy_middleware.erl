-module(ca_cowboy_middleware).

-behaviour(cowboy_middleware).

-export([execute/2]).

execute(Req, Env) ->
    io:fwrite("[middleware]:execute()...~n", []),
    {ok, ReqWithCorsHeaders} = set_cors_headers(Req),
    {ok, ReqWithCorsHeaders, Env}.

%% ===================================================================
%% Helpers
%% ===================================================================

set_headers(Headers, Req) ->
    io:fwrite("[middleware]:set_headers()...~n", []),
    ReqWithHeaders = lists:foldl(fun ({Header, Value},
                                      ReqIn) ->
                                         ReqWithHeader =
                                             cowboy_req:set_resp_header(Header,
                                                                        Value,
                                                                        ReqIn),
                                         ReqWithHeader
                                 end,
                                 Req,
                                 Headers),
    {ok, ReqWithHeaders}.

set_cors_headers(Req) ->
    io:fwrite("[middleware]:set_cors_headers()...~n", []),
    Headers = [{<<"access-control-allow-origin">>, <<"*">>},
               {<<"access-control-allow-methods">>,
                <<"POST, GET, OPTIONS">>},
               {<<"access-control-allow-headers">>,
                <<"Origin, X-Requested-With, Content-Type, "
                  "Accept">>},
               {<<"access-control-max-age">>, <<"1000">>}],
    {ok, Req2} = set_headers(Headers, Req),
    {ok, Req2}.
