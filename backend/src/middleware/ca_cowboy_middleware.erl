%%%-------------------------------------------------------------------
%% @doc Middleware for our REST API
%%
%% The module cowboy_middleware defines a callback interface for Cowboy middlewares.
%% Middlewares process the request sequentially in the order they are configured.
%%
%% official doc: https://ninenines.eu/docs/en/cowboy/2.9/manual/cowboy_middleware/
%%
%% @end
%%%-------------------------------------------------------------------
-module(ca_cowboy_middleware).

-behaviour(cowboy_middleware).

-export([execute/2]).

execute(Req, Env) ->
    {ok, ReqWithCorsHeaders} = set_cors_headers(Req),
    Method = cowboy_req:method(ReqWithCorsHeaders),

    io:fwrite("[middleware (pid~p)]:execute()...~n", [self()]),

    case Method of
        <<"OPTIONS">> ->
            {ok, ReqFinal} = cowboy_req:reply(200, ReqWithCorsHeaders),
            {halt, ReqFinal, Env};
        _ ->
            %% continue as normal
            io:fwrite("[middleware (pid~p)]:execute()...matched as normal.~n", [self()]),
            {ok, ReqWithCorsHeaders, Env}
    end.



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
