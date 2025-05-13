%%%===================================================================
%%% MODULE: ghost_db
%%% DESCRIPTION: Gestion de la base de données pour la plateforme de messagerie
%%%===================================================================

-module(ghost_db).

-export([
    init/0, stop/0, next_id/0, next_user_id/0
]).

-include("ghost_records.hrl").

%%%---------------------------
%%% INITIALISATION
%%%---------------------------
init() ->
    % Démarrer Mnesia si besoin
    case mnesia:system_info(is_running) of
        yes -> ok;
        _ ->
            application:set_env(mnesia, dir, "mnesia_data"),
            (catch mnesia:create_schema([node()])),
            mnesia:start()
    end,

    % Démarrer crypto
    case application:start(crypto) of
        ok -> ok;
        {error, {already_started, crypto}} -> ok;
        {error, _} -> application:ensure_all_started(crypto)
    end,

    % Créer la table principale si besoin
    Tables = mnesia:system_info(tables),
    case lists:member(?TABLE, Tables) of
        true -> ok;
        false ->
            {atomic, ok} = mnesia:create_table(
                ?TABLE,
                [
                    {attributes, record_info(fields, ghost_thread_table)},
                    {disc_copies, [node()]}
                ]
            ),
            ok
    end,

    % Créer la table du compteur si besoin
    case lists:member(?COUNTER, Tables) of
        true -> ok;
        false ->
            {atomic, ok} = mnesia:create_table(
                ?COUNTER,
                [
                    {attributes, record_info(fields, counter)},
                    {disc_copies, [node()]}
                ]
            ),
            ok
    end,

    % Créer la table des utilisateurs si besoin
    case lists:member(?USER_TABLE, Tables) of
        true -> ok;
        false ->
            {atomic, ok} = mnesia:create_table(
                ?USER_TABLE,
                [
                    {attributes, record_info(fields, user)},
                    {disc_copies, [node()]}
                ]
            ),
            ok
    end,

    % Initialiser le compteur à 0 si besoin
    mnesia:activity(transaction, fun() ->
        case mnesia:read({counter, id}) of
            [] -> mnesia:write(#counter{key = id, value = 0});
            _ -> ok
        end
    end),

    % Initialiser le compteur d'utilisateurs si besoin
    mnesia:activity(transaction, fun() ->
        case mnesia:read({counter, user_id}) of
            [] -> mnesia:write(#counter{key = user_id, value = 0});
            _ -> ok
        end
    end),

    ok.

stop() ->
    mnesia:stop().

%%%---------------------------
%%% COMPTEUR D'ID PERSISTANT
%%%---------------------------
next_id() ->
    Fun = fun() ->
        case mnesia:read({counter, id}) of
            [#counter{value = Value}] ->
                NewValue = Value + 1,
                mnesia:write(#counter{key = id, value = NewValue}),
                NewValue;
            [] ->
                mnesia:write(#counter{key = id, value = 1}),
                1
        end
    end,
    {atomic, Id} = mnesia:transaction(Fun),
    Id.

next_user_id() ->
    Fun = fun() ->
        case mnesia:read({counter, user_id}) of
            [#counter{value = Value}] ->
                NewValue = Value + 1,
                mnesia:write(#counter{key = user_id, value = NewValue}),
                NewValue;
            [] ->
                mnesia:write(#counter{key = user_id, value = 1}),
                1
        end
    end,
    {atomic, Id} = mnesia:transaction(Fun),
    Id.