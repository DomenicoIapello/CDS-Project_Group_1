%%%-------------------------------------------------------------------
%% @doc Game Server Route
%% 
%% 
%% documentation of cowboy_rest's behaviour: https://ninenines.eu/docs/en/cowboy/2.9/manual/cowboy_rest/
%%
%% @end
%%%-------------------------------------------------------------------
-module(gameserver_route).

-behaviour(cowboy_rest).

-export([init/2, known_methods/2, allowed_methods/2, content_types_provided/2, content_types_accepted/2]).
-export([response_to_get/2, response_to_post/2]).


%%% ==========================================================================
%%% Initialize REST handler behavior
%%% ==========================================================================

%% -------------------------------------------------------------------------
%% @doc
%% init/2 returns cowboy_rest in its tuple answer to switch to REST handler behavior.
%%
%% @spec content_types_provided(Req, State) -> {Result, Req, State}
%% @end
%% -------------------------------------------------------------------------
init(Req0, State) -> 
    io:fwrite("[gameserver_route.erl (pid~p)]:init() switches to REST handler behavior...~n", [self()]),
    {cowboy_rest, Req0, State}.  

%% -------------------------------------------------------------------------
%% @doc
%% Return the list of known methods.
%%
%% This is the full list of methods known by the server, 
%% regardless of their use in the resource.
%%
%% @spec known_methods(Req, State) -> {Result, Req, State}
%% @end
%% -------------------------------------------------------------------------
known_methods(Req, State) ->
    io:fwrite("[gameserver_route.erl]:known_methods() are returned.~n", []),
    Result = [<<"GET">>, <<"POST">>],
    {Result, Req, State}.

%% -------------------------------------------------------------------------
%% @doc
%% Return the list of allowed methods
%%
%% @spec allowed_methods(Req, State) -> {Result, Req, State}
%% @end
%% -------------------------------------------------------------------------
allowed_methods(Req, State) ->
    io:fwrite("[gameserver_route.erl]:allowed_methods() are returned.~n", []),
    {[<<"GET">>, <<"POST">>], Req, State}.

%% -------------------------------------------------------------------------
%% @doc
%% The media types that we return to the frontend based on a HTTP GET request
%%
%% @spec content_types_provided(Req, State) -> {Result, Req, State}
%% @end
%% -------------------------------------------------------------------------
content_types_provided(Req, State) ->
    io:fwrite("[gameserver_route.erl]:content_types_provided() to a GET.~n", []),
    {
        [{{<<"application">>, <<"json">>, []}, response_to_get}],
        Req,
        State
    }.

%% -------------------------------------------------------------------------
%% @doc
%% The media types that we accept from the frontend based on a HTTP POST request
%%
%% @spec content_types_provided(Req, State) -> {Result, Req, State}
%% @end
%% -------------------------------------------------------------------------
content_types_accepted(Req, State) ->
    io:fwrite("[gameserver_route.erl]:content_types_accepted() from a POST.~n", []),
    {
        [{{<<"application">>, <<"json">>, []}, response_to_post}],
        Req,
        State
    }.



%%% ==========================================================================
%%% Handling the GET / POST
%%% ==========================================================================

%% -------------------------------------------------------------------------
%% @doc
%% 
%%
%% @spec 
%% @end
%% -------------------------------------------------------------------------
response_to_get(Req, State) ->
    io:fwrite("[gameserver_route.erl]:response_to_get()....~n", []),
    QsVals = cowboy_req:parse_qs(Req),
    io:fwrite("[gameserver_route.erl]:response_to_get(): parsed query=~p.~n", [QsVals]),

    {_, Current_Grid} = gameserver_process:get_intial_grid(),
    Reply = {current_grid, Current_Grid},
    io:fwrite("[gameserver_route.erl] Reply to encode is: ~p.~n", [Reply]),
    EncodedReply = jiffy:encode({[Reply]}),
    io:fwrite("[gameserver_route.erl] QsVals is: ~p.~n", [EncodedReply]),
    {EncodedReply, Req, State}.

%% -------------------------------------------------------------------------
%% @doc
%% 
%% To respond to a HTTP POST request from the frontend, the backend do
%% 1) decodes the JSON data into an Erlang data structure
%% 2) send the Erlang data to the backend process (gameserver, also called distributor in our doc/schema)
%% 3) send an answer back to the frontend as a JSON: {player_one_status, player_2_status, stalemate_status},
%%    each a binary (0 or 1)
%%
%% @spec 
%% @end
%% -------------------------------------------------------------------------
response_to_post(Req0, State) ->
    io:fwrite("[gameserver_route.erl] reponse_to_post ...~n", []),
    % Read and decode JSON data into Erlang data
    {ok, EncodedData, _} = cowboy_req:read_body(Req0),
    DecodedData = jiffy:decode(EncodedData),  % DecodedData = {[{<<.>>, <<.>>}]} = a tuple {} containing a list [] containg a tuple {} with two bit strings <<>>.
    io:fwrite("[gameserver_route.erl] DecodedData ~p ...~n", [DecodedData]),
    Decoded_current_grid_as_integers = flatten_decoded_data_v2(DecodedData),

    % Filter out if DecodedData is "acceptable", TODO: add real control and different HTTP code (204, 206) ?
    % if yes, use Erlang backend logic to analyze it.
    case Decoded_current_grid_as_integers of
        [_,_,_,_,_,_,_,_,_] ->
            io:fwrite("[gameserver_route.erl] Analyze any list of length nine...~n", []),
            % a good format response to analyze
            {R, Code} = gameserver_process:analyze_post_request(Decoded_current_grid_as_integers),
            % io:fwrite("[gameserver_route.erl] gameserver_process analyzed the DecodedData, its answer is: ~p with code ~p.~n", [R, Code]),
            Reply = {resp, R}
    end,

    % Format back into JSON for the frontend
    io:fwrite("[gameserver_route.erl] Reply to encode: ~p.~n", [Reply]),
    EncodedReply = jiffy:encode({[Reply]}),
    io:fwrite("[gameserver_route.erl] JSON Encoded Reply: ~p.~n", [EncodedReply]),
    % Send response back to frontend to answer its initial POST request.
    Req1 = cowboy_req:reply(Code,
                     #{<<"Content-Type">> => <<"application/json">>},   % cowboy_req:headers(3)
                     EncodedReply,
                     Req0),
                    
    {ok, Req1, State}.


%%% ==========================================================================
%%% Miscelleanous Helper Functions
%%% ==========================================================================

%% -------------------------------------------------------------------------
%% @doc
%% 
%% The goal is the go from {[{<<"current_grid">>, [1,0,0,2,0,0,0,0,0]}]} to [1,0,0,0,0,0,0,0,0]
%%
%% @spec 
%% @end
%% -------------------------------------------------------------------------
flatten_decoded_data_v2(DecodedData) ->
    % DecodedData = {[{<<.>>, <<.>>}]} = a tuple {} containing a list [] containg a tuple {} with two bit strings <<>>.
    DecodedData_elem1 = element(1, DecodedData), % DecodedData_elem1 = [{<<.>>, <<.>>}]
    [DecodedData_elem1_head | _DecodedData_elem1_body] = DecodedData_elem1, % DecodedData_elem1_head = {<<.>>, <<.>>}
    Decoded_current_grid = element(2, DecodedData_elem1_head), % Decoded_current_grid = <<.>>
    Decoded_current_grid.