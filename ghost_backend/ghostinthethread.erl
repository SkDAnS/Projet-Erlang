-module(ghostinthethread).



-export([

    init/0, stop/0,

    create_user/2, login/2,

    add_message/2, reply_to_message/3,

    like_message/2, unlike_message/2,

    delete_message/2, delete_reply/3,

    get_all_messages_as_string/0

]).



-include("ghost_records.hrl").



init() ->

    ghost_db:init().



stop() ->

    ghost_db:stop().



%%%---------------------------

%%% GESTION DES UTILISATEURS

%%%---------------------------

create_user(Pseudo, Password) ->

    ghost_user:create_user(Pseudo, Password).



login(Pseudo, Password) ->

    ghost_user:login(Pseudo, Password).



%%%---------------------------

%%% MESSAGES

%%%---------------------------

add_message(Pseudo, Text) ->

    ghost_db:init(),

    UserId = ghost_user:get_user_id(Pseudo),

    ghost_message:add_message(UserId, Text).



reply_to_message(Pseudo, ParentId, Text) ->

    ghost_db:init(),

    UserId = ghost_user:get_user_id(Pseudo),

    ghost_message:reply_to_message(UserId, ParentId, Text).



like_message(Pseudo, MsgId) ->

    ghost_db:init(),

    UserId = ghost_user:get_user_id(Pseudo),

    ghost_message:like_message(UserId, MsgId).



unlike_message(Pseudo, MsgId) ->

    ghost_db:init(),

    UserId = ghost_user:get_user_id(Pseudo),

    ghost_message:unlike_message(UserId, MsgId).



delete_message(Pseudo, MsgId) ->

    ghost_db:init(),

    UserId = ghost_user:get_user_id(Pseudo),

    ghost_message:delete_message(UserId, MsgId).



delete_reply(Pseudo, ParentId, ReplyId) ->

    ghost_db:init(),

    UserId = ghost_user:get_user_id(Pseudo),

    ghost_message:delete_reply(UserId, ParentId, ReplyId).



%%%---------------------------

%%% AFFICHAGE

%%%---------------------------

get_all_messages_as_string() ->

    ghost_message:display_all().
