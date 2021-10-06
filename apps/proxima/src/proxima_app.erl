%%%-------------------------------------------------------------------
%% @doc proxima public API
%% @end
%%%-------------------------------------------------------------------

-module(proxima_app).

-behaviour(application).

-export([start/2, start_phase/3, stop/1]).

-include_lib("kernel/include/logger.hrl").

start({takeover, OtherNode}, []) ->
    _ = et:trace_me(80, {proxima_app, erlang:node()}, start, [{takeover, OtherNode}]),

    ?LOG_INFO("Taking over from ~p", [OtherNode]),
    case proxima_sup:start_link() of
        {error, {already_started, Pid}} ->
            {ok, Pid};
        {ok, Pid} ->
            {ok, Pid}
    end;
start({failover, OtherNode}, []) ->
    _ = et:trace_me(80, {proxima_app, erlang:node()}, start, [{failover, OtherNode}]),

    ?LOG_INFO("Failing over proxima"),
    proxima_sup:start_link();
start(normal, _StartArgs) ->
    ?LOG_INFO("Normal proxima"),
    proxima_sup:start_link().

% http://erlang.org/pipermail/erlang-questions/2011-March/057323.html
% http://erlang.org/documentation/doc-4.9.1/doc/design_principles/applications.html
%
% https://erlang.org/doc/design_principles/distributed_applications.html
% init: processes started, basic initialisation
start_phase(init, _Type, []) ->
    ?LOG_INFO("Start phase init: ~p", [_Type]),
    ok;
% takeover: ignored unless application start type was {takeover, FromNode}
start_phase(takeover, {takeover, _FromNode}, []) ->
    ?LOG_INFO("Start phase takeover from ~p", [_FromNode]),
    ok;
start_phase(takeover, _Type, []) ->
    ?LOG_INFO("Start phase takeover: ~p", [_Type]),
    ok;
% go: to register global names, do other forms of complex initialisation, etc.
start_phase(go, Type, []) ->
    _ = et:trace_me(80, {proxima_app, erlang:node()}, start, [{go, Type}]),

    ?LOG_INFO("Start phase go: ~p", [Type]),
    Names = [proxima],
    Fun = fun(Name) ->
        Pid = erlang:whereis(Name),
        yes = global:re_register_name(Name, Pid)
    end,
    lists:foreach(Fun, Names),

    case Type of
        {failover, _Node} ->
            ok;
        {takeover, Node} ->
            ok = proxima:fetch_state(Node);
        normal ->
            ok
    end,

    ok.

stop(_State) ->
    ok.

%% internal functions
