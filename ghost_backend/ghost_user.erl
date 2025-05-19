-module(ghost_user).



-export([

    create_user/2, login/2, get_user_id/1, get_pseudo_by_id/1,

    hash_password/1, verify_password/2

]).



-include("ghost_records.hrl").



%%%---------------------------

%%% GESTION DES UTILISATEURS

%%%---------------------------

create_user(Pseudo, Password) ->

    ghost_db:init(),

    Trans = fun() ->

        Result = mnesia:matchobject(#user{pseudo = Pseudo,  = '_'}),

        case Result of

            [] ->

                UserId = ghost_db:next_user_id(),

                PasswordHash = hash_password(Password),

                User = #user{

                    id = UserId,

                    pseudo = Pseudo,

                    passwordhash = PasswordHash

                },

                mnesia:write(User),

                {ok, UserId};

             ->

                {error, user_already_exists}

        end

    end,

    case mnesia:transaction(Trans) of

        {atomic, {ok, UserId}} -> {ok, UserId};

        {atomic, {error, Reason}} -> {error, Reason};

        {aborted, Reason} -> {error, Reason}

    end.



login(Pseudo, Password) ->

    ghost_db:init(),

    Trans = fun() ->

        Result = mnesia:matchobject(#user{pseudo = Pseudo,  = '_'}),

        case Result of

            [User] ->

                StoredHash = User#user.password_hash,

                case verify_password(Password, StoredHash) of

                    true -> {ok, logged_in};

                    false -> {error, invalid_password}

                end;

            [] -> {error, user_notfound}

        end

    end,

    case mnesia:transaction(Trans) of

        {atomic, {ok, }} -> ok;

        {atomic, {error, Reason}} -> {error, Reason};

        {aborted, Reason} -> {error, Reason}

    end.



get_user_id(Pseudo) ->

    Trans = fun() ->

        case mnesia:matchobject(#user{pseudo = Pseudo,  = '_'}) of

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

    Salt = crypto:strong_rand_bytes(16),

    Digest = crypto:hash(sha256, <<Salt/binary, (list_to_binary(Password))/binary>>),

    base64:encode(<<Salt/binary, Digest/binary>>).



verify_password(Password, StoredHash) ->

    DecodedHash = base64:decode(StoredHash),

    <<Salt:16/binary, StoredDigest/binary>> = DecodedHash,

    Digest = crypto:hash(sha256, <<Salt/binary, (list_to_binary(Password))/binary>>),

    Digest =:= StoredDigest.
