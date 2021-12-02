-module(playerone_route).

-behaviour(cowboy_rest).

-export([init/2, allowed_methods/2, content_types_provided/2, content_types_accepted/2]).
-export([get_move/2, post_move/2, known_methods/2]).

init(Req0, State) -> {cowboy_rest, Req0, State}.

allowed_methods(Req, State) ->
    {[<<"GET">>, <<"POST">>], Req, State}.

content_types_provided(Req, State) ->
    {[{{<<"application">>, <<"json">>, []}, get_move}],
     Req,
     State}.

content_types_accepted(Req, State) ->
    {[{{<<"application">>, <<"json">>, []}, post_move}],
     Req,
     State}.

get_move(Req0, State0) ->
    % Message = Message = {[{response, <<"Hello">>}]},
    QsVals = cowboy_req:parse_qs(Req0),
    Message = Message = {[{response, player_one:get(QsVals)}]},
    {jiffy:encode(Message), Req0, State0}.

post_move(Req0, _State0) ->
    {ok, EncodedData, _} = cowboy_req:read_body(Req0),
    DecodedData = jiffy:decode(EncodedData),

    case DecodedData of
        {[{<<"0000X0000">>, undefined}, {<<"grid">>, undefined}]} ->
            {Reply, Code} = {{response, <<"undefined winner move">>},
                             204}
    end,
    EncodedReply = jiffy:encode({[Reply]}),

    cowboy_req:reply(Code,
                     #{<<"content-type">> => <<"application/json">>},
                     EncodedReply,
                     Req0).

known_methods(Req, State) ->
    Result = [<<"GET">>, <<"POST">>],
    {Result, Req, State}.
