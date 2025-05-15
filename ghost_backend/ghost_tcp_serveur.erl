-module(ghost_tcp_serveur).
-export([start/0]).

start() ->
    {ok, LSock} = gen_tcp:listen(8989, [binary, {packet, 0}, {active, false}, {reuseaddr, true}]),
    io:format("[TCP SERVEUR] Serveur en écoute sur le port 8989...~n"),
    accept_loop(LSock).

accept_loop(LSock) ->
    {ok, Sock} = gen_tcp:accept(LSock),
    spawn(fun() -> handle_client(Sock, undefined) end),
    accept_loop(LSock).

handle_client(Sock, SessionUser) ->
    case gen_tcp:recv(Sock, 0) of
        {ok, Data} ->
            {Continue, NewSession} = handle_request(Sock, binary_to_list(Data), SessionUser),
            case Continue of
                true -> handle_client(Sock, NewSession);
                false -> gen_tcp:close(Sock)
            end;
        {error, closed} ->
            io:format("[TCP SERVEUR] Connexion fermée par le client ~p~n", [SessionUser]),
            ok
    end.

handle_request(Sock, Data, SessionUser) ->
    case string:tokens(Data, ",") of
        ["create_user", User, Pass] ->
            case ghostinthethread:create_user(User, Pass) of
                {ok, _} ->
                    gen_tcp:send(Sock, "user_created"),
                    {true, User};
                {error, Reason} ->
                    gen_tcp:send(Sock, list_to_binary(io_lib:format("create_error: ~p", [Reason]))),
                    {true, SessionUser}
            end;
        ["login", User, Pass] ->
            case ghostinthethread:login(User, Pass) of
                ok ->
                    gen_tcp:send(Sock, "login_success"),
                    {true, User};
                {error, Reason} ->
                    gen_tcp:send(Sock, list_to_binary(io_lib:format("login_error: ~p", [Reason]))),
                    {true, SessionUser}
            end;
        ["add", Text] when SessionUser =/= undefined ->
            case ghostinthethread:add_message(SessionUser, Text) of
                {error, _} -> gen_tcp:send(Sock, "add_failed");
                _ -> gen_tcp:send(Sock, "added")
            end,
            {true, SessionUser};
        ["like", MsgIdStr] when SessionUser =/= undefined ->
            MsgId = list_to_integer(MsgIdStr),
            case ghostinthethread:like_message(SessionUser, MsgId) of
                {atomic, liked} -> gen_tcp:send(Sock, "liked");
                _ -> gen_tcp:send(Sock, "like_failed")
            end,
            {true, SessionUser};
        ["unlike", MsgIdStr] when SessionUser =/= undefined ->
            MsgId = list_to_integer(MsgIdStr),
            case ghostinthethread:unlike_message(SessionUser, MsgId) of
                {atomic, unliked} -> gen_tcp:send(Sock, "unliked");
                _ -> gen_tcp:send(Sock, "unlike_failed")
            end,
            {true, SessionUser};
        ["reply", ParentIdStr, Text] when SessionUser =/= undefined ->
            ParentId = list_to_integer(ParentIdStr),
            case ghostinthethread:reply_to_message(SessionUser, ParentId, Text) of
                {error, _} -> gen_tcp:send(Sock, "reply_failed");
                _ -> gen_tcp:send(Sock, "reply_added")
            end,
            {true, SessionUser};
        ["delete", MsgIdStr] when SessionUser =/= undefined ->
            MsgId = list_to_integer(MsgIdStr),
            case ghostinthethread:delete_message(SessionUser, MsgId) of
                ok -> gen_tcp:send(Sock, "message_deleted");
                _ -> gen_tcp:send(Sock, "delete_failed")
            end,
            {true, SessionUser};
        ["delete_reply", ParentIdStr, ReplyIdStr] when SessionUser =/= undefined ->
            ParentId = list_to_integer(ParentIdStr),
            ReplyId = list_to_integer(ReplyIdStr),
            case ghostinthethread:delete_reply(SessionUser, ParentId, ReplyId) of
                ok -> gen_tcp:send(Sock, "reply_deleted");
                _ -> gen_tcp:send(Sock, "delete_reply_failed")
            end,
            {true, SessionUser};
        ["display"] when SessionUser =/= undefined ->
            case ghostinthethread:get_all_messages_as_string() of
                {ok, MsgStr} ->
                    gen_tcp:send(Sock, iolist_to_binary(MsgStr));
                {error, Reason} ->
                    gen_tcp:send(
                        Sock, iolist_to_binary(io_lib:format("Erreur display: ~p~n", [Reason]))
                    )
            end,
            {true, SessionUser};
        ["logout"] when SessionUser =/= undefined ->
            ghostinthethread:logout(),
            gen_tcp:send(Sock, "logged_out"),
            {true, undefined};
        _ ->
            gen_tcp:send(Sock, "invalid_or_unauthorized_command"),
            {true, SessionUser}
    end.
