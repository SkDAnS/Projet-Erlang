%%%-------------------------------------------------------------------
%%% @doc Main application module
%%% @end
%%%-------------------------------------------------------------------
-module(ghost_app).

-behaviour(application).

-export([start/2, stop/1]).

start(_StartType, _StartArgs) ->
    ghostinthethread:init(),
    ghost_sup:start_link().

stop(_State) ->
    ghostinthethread:stop(),
    ok.