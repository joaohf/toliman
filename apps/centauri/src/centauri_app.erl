%%%-------------------------------------------------------------------
%% @doc centauri public API
%% @end
%%%-------------------------------------------------------------------

-module(centauri_app).

-behaviour(application).

-export([start/2, stop/1]).

-include_lib("kernel/include/logger.hrl").

start(_StartType, _StartArgs) ->
    case application:start(proxima) of
        ok ->
            ok;
        {error, {already_started, Reason}} ->
            ?LOG_WARNING("proxima already started: ~p", [Reason]),
            ok
    end,

    centauri_sup:start_link().

stop(_State) ->
    ok.

%% internal functions
