%%%===================================================================
%%% MODULE: ghostinthethread
%%% DESCRIPTION: Module principal de la plateforme de messagerie sociale
%%%===================================================================

-module(ghostinthethread).

-export([
    init/0, stop/0, add_message/2, reply_to_message/3, like_message/2, unlike_message/2,
    delete_message/2, delete_reply/3, display_all/0, get_all_messages_json/0,
    debug_table/0, add_test_message/0,
    create_user/2, login/2, logout/0
]).

-include("ghost_records.hrl").

%%%---------------------------
%%% INITIALISATION
%%%---------------------------
init() ->
    ghost_db:init(),
    ghost_session:init_process(),
    ok.

stop() ->
    ghost_db:stop().

%%%---------------------------
%%% GESTION DES UTILISATEURS
%%%---------------------------
create_user(Pseudo, Password) ->
    ghost_user:create_user(Pseudo, Password).

login(Pseudo, Password) ->
    ghost_user:login(Pseudo, Password).

logout() ->
    ghost_user:logout().

%%%---------------------------
%%% AJOUTER UN MESSAGE
%%%---------------------------
add_message(_, Text) when not is_list(Text) ->
    {error, invalid_text_format};
add_message(_, Text) ->
    ghost_db:init(),
    case ghost_session:is_logged_in() of
        true ->
            Pseudo = ghost_session:get_current_user(),
            UserId = ghost_user:get_user_id(Pseudo),
            ghost_message:add_message(UserId, Text);
        false ->
            io:format("Vous devez être connecté pour ajouter un message~n"),
            {error, not_logged_in}
    end.

%%%---------------------------
%%% RÉPONDRE À UN MESSAGE
%%%---------------------------
reply_to_message(_, _, Text) when not is_list(Text) ->
    {error, invalid_text_format};
reply_to_message(_, ParentId, Text) ->
    ghost_db:init(),
    case ghost_session:is_logged_in() of
        true ->
            Pseudo = ghost_session:get_current_user(),
            UserId = ghost_user:get_user_id(Pseudo),
            ghost_message:reply_to_message(UserId, ParentId, Text);
        false ->
            io:format("Vous devez être connecté pour répondre à un message~n"),
            {error, not_logged_in}
    end.

%%%---------------------------
%%% GESTION DES LIKES
%%%---------------------------
like_message(_, MsgId) ->
    ghost_db:init(),
    case ghost_session:is_logged_in() of
        true ->
            Pseudo = ghost_session:get_current_user(),
            UserId = ghost_user:get_user_id(Pseudo),
            ghost_message:like_message(UserId, MsgId);
        false ->
            io:format("Vous devez être connecté pour aimer un message~n"),
            {error, not_logged_in}
    end.

unlike_message(_, MsgId) ->
    ghost_db:init(),
    case ghost_session:is_logged_in() of
        true ->
            Pseudo = ghost_session:get_current_user(),
            UserId = ghost_user:get_user_id(Pseudo),
            ghost_message:unlike_message(UserId, MsgId);
        false ->
            io:format("Vous devez être connecté pour ne plus aimer un message~n"),
            {error, not_logged_in}
    end.

%%%---------------------------
%%% SUPPRIMER UN MESSAGE
%%%---------------------------
delete_message(_, MsgId) ->
    ghost_db:init(),
    case ghost_session:is_logged_in() of
        true ->
            Pseudo = ghost_session:get_current_user(),
            UserId = ghost_user:get_user_id(Pseudo),
            ghost_message:delete_message(UserId, MsgId);
        false ->
            io:format("Vous devez être connecté pour supprimer un message~n"),
            {error, not_logged_in}
    end.

%%%---------------------------
%%% SUPPRIMER UNE RÉPONSE
%%%---------------------------
delete_reply(_, ParentId, ReplyId) ->
    ghost_db:init(),
    case ghost_session:is_logged_in() of
        true ->
            Pseudo = ghost_session:get_current_user(),
            UserId = ghost_user:get_user_id(Pseudo),
            ghost_message:delete_reply(UserId, ParentId, ReplyId);
        false ->
            io:format("Vous devez être connecté pour supprimer une réponse~n"),
            {error, not_logged_in}
    end.

%%%---------------------------
%%% AFFICHAGE DES MESSAGES
%%%---------------------------
display_all() ->
    ghost_db:init(),
    case ghost_session:is_logged_in() of
        true ->
            ghost_message:display_all(),
            ok;
        false ->
            io:format("Vous devez être connecté pour afficher les messages~n"),
            {error, not_logged_in}
    end.

%%%---------------------------
%%% FONCTION DEBUG
%%%---------------------------
debug_table() ->
    ghost_db:init(),
    case ghost_session:is_logged_in() of
        true ->
            ghost_message:debug_table();
        false ->
            io:format("Vous devez être connecté pour déboguer la table~n"),
            {error, not_logged_in}
    end.

%%%---------------------------
%%% FONCTION DE TEST
%%%---------------------------
add_test_message() ->
    ghost_db:init(),
    case ghost_session:is_logged_in() of
        true ->
            Pseudo = ghost_session:get_current_user(),
            Timestamp = erlang:system_time(second),
            Message = "Test message " ++ integer_to_list(Timestamp),
            Id = add_message(Pseudo, Message),
            io:format("Message ajouté avec ID: ~p~nTexte: ~s~n", [Id, Message]),
            Id;
        false ->
            io:format("Vous devez être connecté pour ajouter un message de test~n"),
            {error, not_logged_in}
    end.

%%%---------------------------
%%% JSON
%%%---------------------------
get_all_messages_json() ->
    ghost_db:init(),
    ghost_message:get_all_messages_json().