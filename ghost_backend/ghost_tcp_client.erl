-module(ghost_tcp_client).
-export([start_session/0]).

start_session() ->
    case gen_tcp:connect("127.0.0.1", 8989, [binary, {packet, 0}, {active, false}]) of
        {ok, Sock} ->
            loop(Sock);
        {error, Reason} ->
            io:format("[CLIENT] Erreur de connexion: ~p~n", [Reason])
    end.

loop(Sock) ->
    Input = io:get_line("[CLIENT] Entrez une commande > "),
    case string:trim(Input) of
        "exit" ->
            io:format("[CLIENT] Déconnexion manuelle~n"),
            gen_tcp:close(Sock);
        Command ->
            gen_tcp:send(Sock, list_to_binary(Command)),
            case gen_tcp:recv(Sock, 0) of
                {ok, Response} ->
                    case binary_to_list(Response) of
                        "logged_out" ->
                            io:format("[CLIENT] Déconnecté.~n"),
                            gen_tcp:close(Sock);
                        Text ->
                            io:format("[CLIENT] Réponse: ~s~n", [Text]),
                            loop(Sock)
                    end;
                {error, closed} ->
                    io:format("[CLIENT] Connexion fermée par le serveur~n")
            end
    end.
