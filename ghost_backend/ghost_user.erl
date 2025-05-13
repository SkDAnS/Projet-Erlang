%%%===================================================================
%%% MODULE: ghost_user
%%% DESCRIPTION: Gestion des utilisateurs pour la plateforme de messagerie
%%%===================================================================

-module(ghost_user).

-export([
    create_user/2, login/2, logout/0, get_user_id/1, get_pseudo_by_id/1,
    hash_password/1, verify_password/2
]).

-include("ghost_records.hrl").

%%%---------------------------
%%% GESTION DES UTILISATEURS
%%%---------------------------
create_user(Pseudo, Password) ->
    ghost_db:init(),
    % Vérifier si le pseudo existe déjà
    Trans = fun() ->
        Result = mnesia:match_object(#user{pseudo = Pseudo, _ = '_'}),
        case Result of
            [] ->
                % Créer un nouvel utilisateur
                UserId = ghost_db:next_user_id(),
                PasswordHash = hash_password(Password),
                User = #user{
                    id = UserId,
                    pseudo = Pseudo,
                    password_hash = PasswordHash
                },
                mnesia:write(User),
                % Connecter automatiquement l'utilisateur
                Pid = whereis(?SESSION),
                Pid ! {login, Pseudo},
                {ok, UserId};
            _ ->
                {error, user_already_exists}
        end
    end,
    case mnesia:transaction(Trans) of
        {atomic, {ok, UserId}} -> 
            io:format("Utilisateur créé avec succès! ID: ~p~n", [UserId]),
            {ok, UserId};
        {atomic, {error, Reason}} -> 
            io:format("Erreur lors de la création: ~p~n", [Reason]),
            {error, Reason};
        {aborted, Reason} ->
            io:format("Transaction aborted: ~p~n", [Reason]),
            {error, Reason}
    end.

login(Pseudo, Password) ->
    ghost_db:init(),
    Trans = fun() ->
        Result = mnesia:match_object(#user{pseudo = Pseudo, _ = '_'}),
        case Result of
            [User] ->
                StoredHash = User#user.password_hash,
                case verify_password(Password, StoredHash) of
                    true ->
                        Pid = whereis(?SESSION),
                        Pid ! {login, Pseudo},
                        {ok, logged_in};
                    false ->
                        {error, invalid_password}
                end;
            [] ->
                {error, user_not_found}
        end
    end,
    case mnesia:transaction(Trans) of
        {atomic, {ok, _}} -> 
            io:format("Connexion réussie: ~s~n", [Pseudo]),
            ok;
        {atomic, {error, Reason}} -> 
            io:format("Erreur de connexion: ~p~n", [Reason]),
            {error, Reason};
        {aborted, Reason} ->
            io:format("Transaction aborted: ~p~n", [Reason]),
            {error, Reason}
    end.

logout() ->
    Pid = whereis(?SESSION),
    Pid ! logout,
    io:format("Vous êtes maintenant déconnecté~n"),
    ok.

get_user_id(Pseudo) ->
    Trans = fun() ->
        case mnesia:match_object(#user{pseudo = Pseudo, _ = '_'}) of
            [User] -> User#user.id;
            [] -> erlang:error({utilisateur_inexistant, Pseudo})
        end
    end,
    {atomic, UserId} = mnesia:transaction(Trans),
    UserId.

get_pseudo_by_id(UserId) ->
    Trans = fun() ->
        case mnesia:read({?USER_TABLE, UserId}) of
            [User] -> User#user.pseudo;
            [] -> "Anonyme"
        end
    end,
    {atomic, Pseudo} = mnesia:transaction(Trans),
    Pseudo.

hash_password(Password) ->
    % Implémentation simple de hachage avec sha256
    % Dans un système de production, il faudrait utiliser bcrypt ou similaire
    Salt = crypto:strong_rand_bytes(16),
    Digest = crypto:hash(sha256, <<Salt/binary, (list_to_binary(Password))/binary>>),
    base64:encode(<<Salt/binary, Digest/binary>>).

verify_password(Password, StoredHash) ->
    DecodedHash = base64:decode(StoredHash),
    <<Salt:16/binary, StoredDigest/binary>> = DecodedHash,
    Digest = crypto:hash(sha256, <<Salt/binary, (list_to_binary(Password))/binary>>),
    Digest =:= StoredDigest.