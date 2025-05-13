
%%%==============================================================
%%% Fichier : ghost_websocket_server.erl
%%% Description : Démarre le serveur Cowboy avec WebSocket
%%%==============================================================

-module(ghost_websocket_server).
-export([start/0, stop/0]).

start() ->
    ghostinthethread:init(),
    application:ensure_all_started(cowboy),

    Dispatch = cowboy_router:compile([
        {'_', [
            {"/websocket", ghost_websocket_handler, []}
        ]}
    ]),

    {ok, _} = cowboy:start_clear(ghost_http_listener,
        [{port, 8080}],
        #{env => #{dispatch => Dispatch}}
    ),
    io:format("WebSocket server started on port 8080~n").

stop() ->
    cowboy:stop_listener(ghost_http_listener).
