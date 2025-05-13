%%%===================================================================
%%% MODULE: ghost_message
%%% DESCRIPTION: Gestion des messages pour la plateforme de messagerie
%%%===================================================================

-module(ghost_message).

-export([
    add_message/2, reply_to_message/3, like_message/2, unlike_message/2,
    delete_message/2, delete_reply/3, display_all/0, debug_table/0,
    get_all_messages_json/0, escape_json_string/1
]).

-include("ghost_records.hrl").

%%%---------------------------
%%% AJOUTER UN MESSAGE
%%%---------------------------
add_message(UserId, Text) ->
    Id = ghost_db:next_id(),
    Msg = #ghost_thread_table{
        id = Id,
        user_id = UserId,
        text = Text,
        replies = [],
        likes = []
    },
    Fun = fun() -> mnesia:write(Msg) end,
    case mnesia:transaction(Fun) of
        {atomic, ok} -> Id;
        {aborted, Reason} ->
            io:format("Transaction aborted: ~p~n", [Reason]),
            0
    end.

%%%---------------------------
%%% RÉPONDRE À UN MESSAGE
%%%---------------------------
reply_to_message(UserId, ParentId, Text) ->
    ReplyId = ghost_db:next_id(),
    Reply = #ghost_thread_table{
        id = ReplyId,
        user_id = UserId,
        text = Text,
        replies = [],
        likes = []
    },
    Trans = fun() ->
        case mnesia:read({?TABLE, ParentId}) of
            [Parent] ->
                Replies = Parent#ghost_thread_table.replies,
                Updated = Parent#ghost_thread_table{replies = [Reply | Replies]},
                mnesia:write(Updated),
                {ok, ReplyId};
            [] ->
                mnesia:abort({error, parent_not_found})
        end
    end,
    case mnesia:transaction(Trans) of
        {atomic, {ok, Result}} -> Result;
        {aborted, Reason} ->
            io:format("Transaction aborted: ~p~n", [Reason]),
            {error, Reason}
    end.

%%%---------------------------
%%% GESTION DES LIKES
%%%---------------------------
like_message(UserId, MsgId) ->
    Trans = fun() ->
        case mnesia:read({?TABLE, MsgId}) of
            [Msg] ->
                Likes = Msg#ghost_thread_table.likes,
                case lists:member(UserId, Likes) of
                    true -> already_liked;
                    false ->
                        Updated = Msg#ghost_thread_table{likes = [UserId | Likes]},
                        mnesia:write(Updated),
                        liked
                end;
            [] ->
                mnesia:abort({error, message_not_found})
        end
    end,
    mnesia:transaction(Trans).

unlike_message(UserId, MsgId) ->
    Trans = fun() ->
        case mnesia:read({?TABLE, MsgId}) of
            [Msg] ->
                Likes = Msg#ghost_thread_table.likes,
                case lists:member(UserId, Likes) of
                    true ->
                        Updated = Msg#ghost_thread_table{likes = lists:delete(UserId, Likes)},
                        mnesia:write(Updated),
                        unliked;
                    false -> not_liked
                end;
            [] ->
                mnesia:abort({error, message_not_found})
        end
    end,
    mnesia:transaction(Trans).

%%%---------------------------
%%% SUPPRIMER UN MESSAGE
%%%---------------------------
delete_message(UserId, MsgId) ->
    Trans = fun() ->
        case mnesia:read({?TABLE, MsgId}) of
            [Msg] ->
                case Msg#ghost_thread_table.user_id of
                    UserId ->
                        mnesia:delete({?TABLE, MsgId}),
                        ok;
                    _ ->
                        mnesia:abort({error, unauthorized})
                end;
            [] ->
                mnesia:abort({error, message_not_found})
        end
    end,
    case mnesia:transaction(Trans) of
        {atomic, ok} -> ok;
        {aborted, Reason} ->
            io:format("Transaction aborted: ~p~n", [Reason]),
            {error, Reason}
    end.

%%%---------------------------
%%% SUPPRIMER UNE RÉPONSE
%%%---------------------------
delete_reply(UserId, ParentId, ReplyId) ->
    Trans = fun() ->
        case mnesia:read({?TABLE, ParentId}) of
            [Parent] ->
                Replies = Parent#ghost_thread_table.replies,
                case lists:keyfind(ReplyId, #ghost_thread_table.id, Replies) of
                    false ->
                        mnesia:abort({error, reply_not_found});
                    Reply ->
                        case Reply#ghost_thread_table.user_id of
                            UserId ->
                                UpdatedReplies = lists:delete(Reply, Replies),
                                UpdatedParent = Parent#ghost_thread_table{replies = UpdatedReplies},
                                mnesia:write(UpdatedParent),
                                ok;
                            _ ->
                                mnesia:abort({error, unauthorized})
                        end
                end;
            [] ->
                mnesia:abort({error, parent_not_found})
        end
    end,
    case mnesia:transaction(Trans) of
        {atomic, ok} -> ok;
        {aborted, Reason} ->
            io:format("Transaction aborted: ~p~n", [Reason]),
            {error, Reason}
    end.

%%%---------------------------
%%% AFFICHAGE DES MESSAGES
%%%---------------------------
display_all() ->
    case mnesia:transaction(fun() -> mnesia:match_object(#ghost_thread_table{_ = '_'}) end) of
        {atomic, Messages} ->
            SortedMessages = lists:sort(fun(A, B) -> A#ghost_thread_table.id < B#ghost_thread_table.id end, Messages),
            case SortedMessages of
                [] ->
                    io:format("~nAucun message à afficher.~n");
                _ ->
                    lists:foreach(
                        fun(#ghost_thread_table{
                            id = Id,
                            user_id = Uid,
                            text = Txt,
                            likes = Likes,
                            replies = Replies
                        }) ->
                            Pseudo = ghost_user:get_pseudo_by_id(Uid),
                            io:format("~n~s [~p] (ID: ~p):~n", [Pseudo, Uid, Id]),
                            io:format("| ~s~n", [Txt]),
                            io:format("| ❤️\tLIKES: ~p~n", [length(Likes)]),
                            print_replies(Replies)
                        end,
                        SortedMessages
                    )
            end;
        {aborted, Reason} ->
            io:format("Transaction aborted: ~p~n", [Reason])
    end.

print_replies([]) -> ok;
print_replies(Replies) ->
    io:format("|~n"),
    lists:foreach(
        fun(#ghost_thread_table{id = RId, user_id = RUid, text = RTxt}) ->
            Pseudo = ghost_user:get_pseudo_by_id(RUid),
            io:format("|_____ [Réponse ID: ~p] ~s: ~s~n", [RId, Pseudo, RTxt])
        end,
        Replies
    ).

%%%---------------------------
%%% FONCTION DEBUG
%%%---------------------------
debug_table() ->
    case mnesia:transaction(fun() -> mnesia:match_object(#ghost_thread_table{_ = '_'}) end) of
        {atomic, Data} ->
            io:format("~nContenu complet de la table :~n~n"),
            lists:foreach(fun(Msg) -> io:format("~p~n", [Msg]) end, Data),
            ok;
        {aborted, Reason} ->
            io:format("Transaction aborted: ~p~n", [Reason]),
            {error, Reason}
    end.

%%%---------------------------
%%% JSON
%%%---------------------------
get_all_messages_json() ->
    case mnesia:transaction(fun() -> mnesia:match_object(#ghost_thread_table{_ = '_'}) end) of
        {atomic, Messages} ->
            MsgsJson = lists:map(fun(M) ->
                RepliesJson = lists:map(fun(R) ->
                    RPseudo = catch ghost_user:get_pseudo_by_id(R#ghost_thread_table.user_id),
                    EscapedText = escape_json_string(R#ghost_thread_table.text),
                    io_lib:format("{\"id\":~p,\"pseudo\":\"~s\",\"text\":\"~s\"}",
                                  [R#ghost_thread_table.id, RPseudo, EscapedText])
                end, M#ghost_thread_table.replies),
                EscapedMainText = escape_json_string(M#ghost_thread_table.text),
                io_lib:format("{\"id\":~p,\"pseudo\":\"~s\",\"text\":\"~s\",\"likes\":~p,\"replies\":[~s]}",
                              [M#ghost_thread_table.id, catch ghost_user:get_pseudo_by_id(M#ghost_thread_table.user_id),
                               EscapedMainText, length(M#ghost_thread_table.likes),
                               string:join(RepliesJson, ",")])
            end, Messages),
            lists:flatten("[" ++ string:join(MsgsJson, ",") ++ "]");
        {aborted, Reason} ->
            io:format("Transaction aborted: ~p~n", [Reason]),
            "[]"
    end.

escape_json_string(Str) ->
    Escaped = lists:foldl(
        fun({Pattern, Replacement}, Acc) ->
            re:replace(Acc, Pattern, Replacement, [global, {return, list}])
        end,
        Str,
        [
            {"\\\\", "\\\\\\\\"},
            {"\"", "\\\\\""},
            {"\n", "\\\\n"},
            {"\r", "\\\\r"},
            {"\t", "\\\\t"},
            {"\b", "\\\\b"},
            {"\f", "\\\\f"}
        ]
    ),
    Escaped.