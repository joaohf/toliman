-module(proxima).

-behaviour(gen_server).

-export([
    start_link/0,
    hello/2,
    fetch_state/1,
    get_state/1,
    pong/2
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
    starts = #{}
}).

-type state() :: map().

-include_lib("kernel/include/logger.hrl").

start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

%% @doc Syncronize a gen_server state from remote node to the local node.
-spec fetch_state(node()) -> ok.
fetch_state(Node) ->
    _ = et:trace_me(20, proxima, fetch_state, [{node, Node}]),

    gen_server:call(?MODULE, {fetch_state, Node}).

%% @doc Returns the gen_server state.
-spec get_state(node()) -> {ok, state()}.
get_state(Node) ->
    FromNode = erlang:node(),

    _ = et:trace_me(20, proxima, get_state, [{node, Node}, {from_node, FromNode}]),

    gen_server:call({?MODULE, Node}, {get_state, FromNode}).

%% @doc Sends a hello informing that a new centauri node is up.
-spec hello(node(), any()) -> ok.
hello(Node, Options) ->
    _ = et:trace_me(30, {centauri, erlang:node()}, proxima, hello, [
        {node, Node}, {options, Options}
    ]),

    RequestId = gen_server:send_request({global, ?MODULE}, {hello, Node, Options}),
    gen_server:wait_response(RequestId, 5000).

%% @doc Replies a ping/1 call.
-spec pong(pid(), node()) -> ok.
pong(From, Node) ->
    _ = et:trace_me(30, {centauri, erlang:node()}, proxima, pong, [{node, Node}]),

    RequestId = gen_server:send_request({global, ?MODULE}, {pong, From, Node}),
    gen_server:wait_response(RequestId, 5000).

init([]) ->
    {ok, #state{}}.

handle_continue(_Continue, State) ->
    {noreply, State}.

handle_call({fetch_state, Node}, _From, State) ->
    {ok, NState} = proxima:get_state(Node),

    ?LOG_INFO("Current state ~p New state ~p from ~p", [State, NState, Node]),

    {reply, ok, NState};
handle_call({get_state, FromNode}, _From, State) ->
    ?LOG_INFO("Receive a get state request: ~p from node ~p", [State, FromNode]),

    {reply, {ok, State}, State};
handle_call({hello, Node, Options}, _From, State) ->
    NState = do_hello(Node, Options, State),

    ok = centauri:ping(Node),

    {reply, ok, NState};
handle_call({pong, From, Node}, _From, State) ->
    NState = do_pong(From, Node, State),

    {reply, ok, NState};
handle_call(_Request, _From, State) ->
    {reply, ok, State}.

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

do_hello(Node, Options, State) ->
    M = maps:put(Node, Options, State#state.starts),

    ?LOG_INFO("hello from ~p options ~p stars ~p", [Node, Options, M]),

    State#state{starts = M}.

do_pong(From, Node, State) ->
    M = maps:update(Node, From, State#state.starts),

    ?LOG_INFO("pong from ~p pid ~p stars ~p", [Node, From, M]),

    State#state{starts = M}.
