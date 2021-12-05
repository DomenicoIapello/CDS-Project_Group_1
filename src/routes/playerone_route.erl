-module(playerone_route).

-behaviour(cowboy_rest).

-export([init/2, allowed_methods/2, content_types_provided/2, content_types_accepted/2]).
-export([get_move/2, post_move/2, known_methods/2]).

%
% INIT
%

% the Req0 (or Req later on) is a request object. It contains info about request, and
% will eventually contain info that is sent back to the browser.
init(Req0, State) -> 
    io:fwrite("[playerone_route.erl]:init()...~n", []),
    {cowboy_rest, Req0, State}.  

known_methods(Req, State) ->
    io:fwrite("[playerone_route.erl]:known_methods()...~n", []),
    Result = [<<"GET">>, <<"POST">>],
    {Result, Req, State}.

allowed_methods(Req, State) ->
    io:fwrite("[playerone_route.erl]:allowed_methods()...~n", []),
    {[<<"GET">>, <<"POST">>], Req, State}.

content_types_provided(Req, State) ->
    io:fwrite("[playerone_route.erl]:content_types_provided()...~n", []),
    {[{{<<"application">>, <<"json">>, []}, get_move}],
     Req,
     State}.

content_types_accepted(Req, State) ->
    io:fwrite("[playerone_route.erl]:content_types_accepted()...~n", []),
    {[{{<<"application">>, <<"json">>, []}, post_move}],
     Req,
     State}.

%
% GET / POST
%
get_move(Req0, State0) ->
    io:fwrite("[playerone_route.erl]:get_move()....~n", []),
    QsVals = cowboy_req:parse_qs(Req0),
    io:fwrite("[playerone_route.erl]:get_move(): parsed query=~p.~n", [QsVals]),
    case lists:keyfind(<<"P1Move">>, 1, QsVals) of
        {_, <<"X___X___X">>} ->
            Message = {[{validmove, <<"Player 1 made a valid move!">>}]};
        {_, <<"X________">>} ->
            Message = {[{winningmove, <<"Player 1 is the winner!">>}]};
        {_, _} ->
            Message = {[{invalidmove, <<"Please reconsider your move. It is invalid.">>}]};
        false -> 
            Message = {[{error, <<"URL or Route not correct. Please verify your input.">>}]}
    end,
    io:fwrite("[playerone_route.erl] QsVals is: ~p.~n", [QsVals]),
    {jiffy:encode(Message), Req0, State0}.

post_move(Req0, _State0) ->
    io:fwrite("[playerone_route.erl]:post_move()...~n", []),
    {ok, EncodedData, _} = cowboy_req:read_body(Req0),
    DecodedData = jiffy:decode(EncodedData),
    io:fwrite("[playerone_route.erl] post_move has decodedata: ~p.~n", [DecodedData]),

    case DecodedData of
        {[{<<"player">>, undefined}]} ->
            {Reply, Code} = {{response, <<"undefined isbn">>}, 206};
        {[{<<"player">>, _}]} ->
            {Reply, Code} = {{response, <<"undefined book">>}, 206};
        {[{<<"player">>, <<"PlayerOne">>}, {<<"move">>, <<"X___X___X">>}]} ->
            {R, Code} = playerone_process:post(<<"PlayerOne">>, <<"X___X___X">>),
            io:format("POST from process: msg=~p with code=~p.~n", [R, Code]),
            Reply = {response, R};
        false ->
            Code = 500,
            Reply = {error, "Incorrent input. Please verify and try again."}
    end,

    EncodedReply = jiffy:encode({[Reply]}),
    io:fwrite("[playerone_route.erl] post_move has EncodedReply: ~p.~n", [EncodedReply]),

    cowboy_req:reply(Code,
                     #{<<"content-type">> => <<"application/json">>},
                     EncodedReply,
                     Req0).
