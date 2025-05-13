%%%==============================================================
%%% Fichier : ghost_websocket_handler.erl
%%% Description : Gère les connexions WebSocket avec Cowboy
%%%==============================================================

-module(ghost_websocket_handler).
-export([init/2, websocket_init/1, websocket_handle/2, websocket_info/2]).

-record(session, {
    pseudo,
    logged_in = false
}).

init(Req, State) ->
    {cowboy_websocket, Req, State, #{idle_timeout => 600000}}.

websocket_init(State) ->
    ghostinthethread:init(),
    {ok, State}.

websocket_handle({text, Msg}, State) ->
    handle_message(Msg, State);
websocket_handle(_Frame, State) ->
    {ok, State}.

websocket_info({send, Data}, State) ->
    {reply, {text, Data}, State};
websocket_info(_Info, State) ->
    {ok, State}.

handle_message(Msg, State) ->
    io:format("🔍 Message WebSocket reçu: ~s~n", [Msg]),
    try
        Decoded = jsx:decode(Msg, [return_maps]),
        Type = maps:get(<<"type">>, Decoded),
        Data = maps:get(<<"data">>, Decoded, undefined),
        Response = process_request(Type, Data),
        {reply, {text, Response}, State}
    catch
        E:R -> 
            io:format(" Exception dans handle_message: ~p:~p~n", [E, R]),
            {ok, State}
    end.


process_request(<<"get_messages">>, _) ->
    try
        MessagesJson = ghostinthethread:get_all_messages_json(),
        jsx:encode(#{
            type => <<"messages">>,
            data => jsx:decode(list_to_binary(MessagesJson), [return_maps])
        })
    catch
        _:Reason ->
            jsx:encode(#{
                type => <<"error">>,
                message => <<"get_messages_failed">>,
                reason => list_to_binary(io_lib:format("~p", [Reason]))
            })
    end;


process_request(<<"get_session">>, _) ->
    Session = case ghostinthethread:get_current_session() of
        #session{pseudo = Pseudo, logged_in = true} -> 
            #{pseudo => Pseudo, logged_in => true};
        _ -> 
            #{pseudo => null, logged_in => false}
    end,
    jsx:encode(#{type => <<"session">>, data => Session});

process_request(<<"login">>, Data) ->
    Pseudo = maps:get(<<"pseudo">>, Data),
    Password = maps:get(<<"password">>, Data),
    Result = ghostinthethread:login(binary_to_list(Pseudo), binary_to_list(Password)),
    Response = case Result of
        ok -> #{success => true};
        {error, Reason} -> #{success => false, error => list_to_binary(atom_to_list(Reason))}
    end,
    jsx:encode(#{type => <<"login_response">>, data => Response});

process_request(<<"logout">>, _) ->
    ghostinthethread:logout(),
    jsx:encode(#{type => <<"session">>, data => #{pseudo => null, logged_in => false}});

process_request(_, _) ->
    jsx:encode(#{type => <<"error">>, message => <<"Unknown request type">>}).
