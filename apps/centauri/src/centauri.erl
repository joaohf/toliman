-module(centauri).

-behaviour(gen_server).

-export([
    start_link/0,
    ping/1
]).

-export([
    init/1,
    handle_continue/2,
    handle_call/3,
    handle_cast/2,
    handle_info/2,
    terminate/2,
    code_change/3
]).

-record(state, {
    proxima_mon,
    proxima_pid,
    tmo = 0
}).

-include_lib("kernel/include/logger.hrl").

start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

%% @doc Sends a ping message to Node.
-spec ping(node()) -> ok.
ping(Node) ->
    From = self(),
    FromNode = erlang:node(),

    _ = et:trace_me(30, proxima, {centauri, Node}, ping, [{node, Node}, {from_node, FromNode}]),

    gen_server:cast({?MODULE, Node}, {ping, From, FromNode}).

init([]) ->
    {ok, #state{}, {continue, say_hello}}.

handle_continue(say_hello, State) ->
    case proxima:hello(erlang:node(), []) of
        {reply, ok} ->
            {noreply, State};
        timeout ->
            {noreply, State, 500};
        {error, {noproc, _}} ->
            {noreply, State, 500}
    end.

handle_call(_Request, _From, State) ->
    {reply, ok, State}.

handle_cast({ping, From, Node}, State) ->
    ?LOG_INFO("ping from pid ~p node ~p", [From, Node]),

    MonitorRef = erlang:monitor(process, From),

    {reply, ok} = proxima:pong(self(), erlang:node()),

    {noreply, State#state{proxima_mon = MonitorRef, proxima_pid = From}};
handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info(
    {'DOWN', MonitorRef, process, Pid, Info},
    #state{proxima_mon = MonitorRef, proxima_pid = Pid} = State
) ->
    _ = et:trace_me(35, {centauri, erlang:node()}, 'DOWN', [proxima]),

    ?LOG_INFO("proxima is down: ~p", [Info]),

    {noreply, State#state{proxima_mon = undefined, proxima_pid = undefined}, {continue, say_hello}};
handle_info(timeout, #state{proxima_pid = undefined, tmo = Tmo} = State) ->
    ?LOG_INFO("timeout waiting hello, tries ~p", [Tmo]),

    _ = et:trace_me(35, {centauri, erlang:node()}, timeout, [Tmo]),

    {noreply, State#state{tmo = Tmo + 1}, {continue, say_hello}};
handle_info(timeout, State) ->
    ?LOG_INFO("got ping"),

    _ = et:trace_me(35, {centauri, erlang:node()}, got_ping, []),

    {noreply, State#state{tmo = 0}}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.
