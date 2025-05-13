%%%===================================================================
%%% MODULE: ghost_session
%%% DESCRIPTION: Gestion des sessions pour la plateforme de messagerie
%%%===================================================================

-module(ghost_session).

-export([
    init_process/0, get_current_session/0, is_logged_in/0, 
    get_current_user/0, session_loop/1
]).

-include("ghost_records.hrl").

%%%---------------------------
%%% GESTION DE SESSION
%%%---------------------------
init_process() ->
    process_flag(trap_exit, true),
    case whereis(?SESSION) of
        undefined ->
            register(?SESSION, spawn(fun() -> session_loop(#session{}) end));
        _ -> ok
    end.

session_loop(Session) ->
    receive
        {get_session, Pid} ->
            Pid ! {session, Session},
            session_loop(Session);
        {update_session, NewSession} ->
            session_loop(NewSession);
        {login, Pseudo} ->
            session_loop(Session#session{pseudo = Pseudo, logged_in = true});
        logout ->
            session_loop(#session{});
        stop ->
            exit(normal);
        _ ->
            session_loop(Session)
    end.

get_current_session() ->
    Pid = whereis(?SESSION),
    Pid ! {get_session, self()},
    receive
        {session, Session} -> Session
    after 1000 ->
        {error, session_timeout}
    end.

is_logged_in() ->
    case get_current_session() of
        #session{logged_in = true} -> true;
        _ -> false
    end.

get_current_user() ->
    case get_current_session() of
        #session{logged_in = true, pseudo = Pseudo} -> Pseudo;
        _ -> undefined
    end.