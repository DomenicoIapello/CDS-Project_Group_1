%%%-------------------------------------------------------------------
%% @doc Game Server Route
%%
%%
%% @end
%%%-------------------------------------------------------------------
-module(gameserver_route).

-behaviour(cowboy_rest).

-export([init/2, allowed_methods/2, content_types_provided/2, content_types_accepted/2]).
-export([get_stalemate_status/2, multicast_current_grid/2, known_methods/2]).

%
% INIT
%

% the Req0 (or Req later on) is a request object. It contains info about request, and
% will eventually contain info that is sent back to the browser.
init(Req0, State) -> 
    io:fwrite("[gameserver_route.erl]:init()...~n", []),
    {cowboy_rest, Req0, State}.  

known_methods(Req, State) ->
    io:fwrite("[gameserver_route.erl]:known_methods()...~n", []),
    Result = [<<"GET">>, <<"POST">>],
    {Result, Req, State}.

allowed_methods(Req, State) ->
    io:fwrite("[gameserver_route.erl]:allowed_methods()...~n", []),
    {[<<"GET">>, <<"POST">>], Req, State}.

content_types_provided(Req, State) ->
    io:fwrite("[gameserver_route.erl]:content_types_provided()...~n", []),
    {[{{<<"application">>, <<"json">>, []}, get_stalemate_status}],
     Req,
     State}.

content_types_accepted(Req, State) ->
    io:fwrite("[gameserver_route.erl]:content_types_accepted()...~n", []),
    {
        [{{<<"application">>, <<"json">>, []}, multicast_current_grid}],
        Req,
        State
    }.

%
% GET / POST
%
get_stalemate_status(Req0, State0) ->
    io:fwrite("[gameserver_route.erl]:get_stalemate_status()....~n", []),
    QsVals = cowboy_req:parse_qs(Req0),
    io:fwrite("[gameserver_route.erl]:get_stalemate_status(): parsed query=~p.~n", [QsVals]),
    case lists:keyfind(<<"P2Move">>, 1, QsVals) of
        {_, <<"O___O___O">>} ->
            Message = {[{validmove, <<"Player 2 made a valid move!">>}]};
        {_, <<"O________">>} ->
            Message = {[{winningmove, <<"Player 2 is the winner!">>}]};
        {_, _} ->
            Message = {[{invalidmove, <<"Please reconsider your move. It is invalid.">>}]};
        false -> 
            Message = {[{error, <<"URL or Route not correct. Please verify your input.">>}]}
    end,
    io:fwrite("[gameserver_route.erl] QsVals is: ~p.~n", [QsVals]),
    {jiffy:encode(Message), Req0, State0}.

multicast_current_grid(Req0, _State0) ->
    io:fwrite("[gameserver_route.erl]:multicast_current_grid()...~n", []),
    {ok, EncodedData, _} = cowboy_req:read_body(Req0),
    DecodedData = jiffy:decode(EncodedData),
    io:fwrite("[gameserver_route.erl] multicast_current_grid has decodedata: ~p.~n", [DecodedData]),

    {Reply, Code} = {{response, <<"wins">>}, 206},
    EncodedReply = jiffy:encode({[Reply]}),

    cowboy_req:reply(Code,
                     #{<<"content-type">> => <<"application/json">>},
                     EncodedReply,
                     Req0).