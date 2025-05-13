%%%===================================================================
%%% FILE: ghost_records.hrl
%%% DESCRIPTION: Définitions de records pour la plateforme de messagerie
%%%===================================================================

%%%---------------------------
%%% RECORDS
%%%---------------------------
-record(ghost_thread_table, {
    id,
    user_id,
    text,
    replies = [],
    likes = []
}).

-record(counter, {key, value}).

-record(user, {
    id,
    pseudo,
    password_hash
}).

-record(session, {
    pseudo,
    logged_in = false
}).

%%%---------------------------
%%% CONSTANTES
%%%---------------------------
-define(TABLE, ghost_thread_table).
-define(COUNTER, counter).
-define(USER_TABLE, user).
-define(SESSION, session).