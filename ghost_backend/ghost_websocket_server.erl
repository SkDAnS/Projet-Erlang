-module(ghost_websocket_server).
-export([start/0, stop/0]).

start() ->
    ghostinthethread:init(),
    application:ensure_all_started(cowboy),

    Dispatch = [
        {'_', [
            {"/websocket", ghost_websocket_handler, []}
        ]}
    ],

    {ok, _} = cowboy:start_clear(ghost_http_listener,
        [{port, 8080}],
        #{env => #{dispatch => cowboy_router:compile(Dispatch)}}
    ),
    io:format("WebSocket server started on port 8080~n"),
    ok.

stop() ->
    cowboy:stop_listener(ghost_http_listener).
