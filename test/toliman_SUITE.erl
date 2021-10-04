-module(toliman_SUITE).

%% Test server callbacks
-export([
    suite/0,
    all/0,
    groups/0,
    init_per_suite/1,
    end_per_suite/1,
    init_per_group/2,
    end_per_group/2,
    init_per_testcase/2,
    end_per_testcase/2
]).

-export([
    t_a_b_failover/1,
    t_a_b_hard_failover/1,
    t_a_b_with_takeover/1
]).

-include_lib("common_test/include/ct.hrl").
-include_lib("eunit/include/eunit.hrl").

-record(centauri_node, {
    ipv4_address :: inet:ip4_address(),
    type :: atom(),
    name :: atom(),
    node :: node()
}).

%%--------------------------------------------------------------------
%% COMMON TEST CALLBACK FUNCTIONS
%%--------------------------------------------------------------------

all() ->
    [
        t_a_b_failover,
        t_a_b_hard_failover,
        t_a_b_with_takeover
    ].

groups() ->
    [].

suite() ->
    [
        {ct_hooks, []},
        {timetrap, {seconds, 300}}
    ].

init_per_suite(Config) ->
    Config.

end_per_suite(_Config) ->
    ok.

init_per_group(_Group, Config) ->
    Config.

end_per_group(_Group, _Config) ->
    ok.

init_per_testcase(_, Config) ->
    Config.

end_per_testcase(_TestCase, _Config) ->
    ok.

%%--------------------------------------------------------------------
%% TEST CASES
%%--------------------------------------------------------------------

t_a_b_failover(Config) ->
    % start normal
    Nodes = start_slaves(Config, [a, b]),

    ct:sleep(3000),

    NodeA = lists:keyfind(a, #centauri_node.name, Nodes),
    NodeB = lists:keyfind(b, #centauri_node.name, Nodes),

    start_application(NodeA, centauri),
    start_application(NodeB, centauri),

    ct:sleep(2000),

    {status, PidA, _, _} = ct_rpc:call(NodeA#centauri_node.node, sys, get_status, [proxima]),
    PidA = ct_rpc:call(NodeB#centauri_node.node, global, whereis_name, [proxima]),

    % B failing over A
    stop_slave(NodeA),

    ct:sleep(2000),

    {status, _PidB, _, _} = ct_rpc:call(NodeB#centauri_node.node, sys, get_status, [proxima]),

    % A is taking over B
    [SecondNodeA] = start_slaves(Config, [a]),
    start_application(SecondNodeA, centauri),

    ct:sleep(2000),

    {status, SecondPidA, _, _} = ct_rpc:call(SecondNodeA#centauri_node.node, sys, get_status, [
        proxima
    ]),
    SecondPidA = ct_rpc:call(NodeB#centauri_node.node, global, whereis_name, [proxima]),

    ct:sleep(2000),

    stop_slave(NodeB),
    stop_slave(SecondNodeA),

    ok.

t_a_b_hard_failover(Config) ->
    % start normal
    Nodes = start_slaves(Config, [a, b]),

    ct:sleep(3000),

    NodeA = lists:keyfind(a, #centauri_node.name, Nodes),
    NodeB = lists:keyfind(b, #centauri_node.name, Nodes),

    start_application(NodeA, centauri),
    start_application(NodeB, centauri),

    ct:sleep(2000),

    {status, PidA, _, _} = ct_rpc:call(NodeA#centauri_node.node, sys, get_status, [proxima]),
    PidA = ct_rpc:call(NodeB#centauri_node.node, global, whereis_name, [proxima]),

    % B failing over A
    _ = ct_rpc:call(NodeA#centauri_node.node, erlang, halt, []),

    ct:sleep(2000),

    {status, _PidB, _, _} = ct_rpc:call(NodeB#centauri_node.node, sys, get_status, [proxima]),

    % A is taking over B
    [SecondNodeA] = start_slaves(Config, [a]),
    start_application(SecondNodeA, centauri),

    ct:sleep(2000),

    {status, SecondPidA, _, _} = ct_rpc:call(SecondNodeA#centauri_node.node, sys, get_status, [
        proxima
    ]),
    SecondPidA = ct_rpc:call(NodeB#centauri_node.node, global, whereis_name, [proxima]),

    stop_slave(NodeB),
    stop_slave(SecondNodeA),

    ok.

t_a_b_with_takeover(Config) ->
    % start normal
    Nodes = start_slaves(Config, [a, b]),

    ct:sleep(3000),

    NodeA = lists:keyfind(a, #centauri_node.name, Nodes),
    NodeB = lists:keyfind(b, #centauri_node.name, Nodes),

    start_application(NodeA, centauri),
    start_application(NodeB, centauri),

    ct:sleep(2000),

    % check if proxima has been started on NodeA
    {status, PidA, _, _} = ct_rpc:call(NodeA#centauri_node.node, sys, get_status, [proxima]),
    PidA = ct_rpc:call(NodeA#centauri_node.node, global, whereis_name, [proxima]),

    % and NodeB knows where is proxima
    PidA = ct_rpc:call(NodeB#centauri_node.node, global, whereis_name, [proxima]),

    ct:sleep(2000),

    ct:pal("Node B takeover"),

    ok = ct_rpc:call(NodeB#centauri_node.node, application, takeover, [proxima, permanent]),

    ct:sleep(2000),

    {status, PidB, _, _} = ct_rpc:call(NodeB#centauri_node.node, sys, get_status, [proxima]),
    PidB = ct_rpc:call(NodeB#centauri_node.node, global, whereis_name, [proxima]),

    ct:pal("Node A takeover"),

    ok = ct_rpc:call(NodeA#centauri_node.node, application, takeover, [proxima, permanent]),

    {status, PidA1, _, _} = ct_rpc:call(NodeA#centauri_node.node, sys, get_status, [proxima]),
    PidA1 = ct_rpc:call(NodeA#centauri_node.node, global, whereis_name, [proxima]),

    ct:sleep(2000),

    stop_slave(NodeB),
    stop_slave(NodeA),

    ok.
%%--------------------------------------------------------------------
%% INTERNAL FUNCTIONS
%%--------------------------------------------------------------------
ip_address(#centauri_node{} = R) ->
    ip_address(R#centauri_node.name);
ip_address(a) ->
    {ok, "127.0.0.7"};
ip_address(b) ->
    {ok, "127.0.0.8"}.

node_name(Name) ->
    {ok, Hostname} = inet:gethostname(),
    {ok, {hostent, FullHostname, _, inet, _, [_]}} = inet:gethostbyname(Hostname),
    HostnameWithSlot = io_lib:format("~s@~s", [atom_to_list(Name), FullHostname]),
    erlang:list_to_atom(lists:flatten(HostnameWithSlot)).

config(_Config, {[NodeA, NodeB], DataDir}, kernel) ->
    fun(Fd, SyncNodesTimeout) ->
        A = node_name(NodeA),
        B = node_name(NodeB),
        LogFilename = filename:join(DataDir, "toliman.log"),

        io:format(
            Fd,
            "[{kernel,[~n"
            "    {distributed, [{proxima, 1000, ['~s', '~s']}]},~n"
            "    {sync_nodes_mandatory, []},~n"
            "    {sync_nodes_optional, ['~s','~s']},~n"
            "    {sync_nodes_timeout, ~w},~n"
            "~n"
            "    {logger_level, info},~n"
            "    {logger,[~n"
            "        {handler, default, logger_std_h, #{~n"
            "            formatter => {logger_formatter, #{ template => [time,\" \", mfa,\":\",line,\" \",level,\": \",msg,\"\\n\"] }},~n"
            "            config => #{file => \"~s\"}}}~n"
            "    ]}~n"
            "]}].~n",
            [A, B, A, B, SyncNodesTimeout, LogFilename]
        )
    end.

from(H, [H | T]) -> T;
from(H, [_ | T]) -> from(H, T);
from(_H, []) -> [].

start_slaves(Config, Nodes) ->
    start_slaves(Config, Nodes, []).

start_slaves(_Config, [], Acc) ->
    Acc;
start_slaves(Config, [NodeName | T], Acc) ->
    DataDir = data_dir(Config, NodeName),
    ok = make_dir(DataDir),

    SysConfig = filename:join(DataDir, "sys.config"),

    Pa = string:join(["-pa" | search_paths()], " "),

    StartArgs = ["-kernel start_dist_ac true -config ", SysConfig, " ", Pa],

    {ok, Fd} = file:open(SysConfig, [write]),

    (config(Config, {[a, b], DataDir}, kernel))(Fd, 1000),

    ok = file:close(Fd),

    {ok, Addr} = ip_address(NodeName),
    {ok, IPv4Address} = inet:parse_ipv4_address(Addr),

    ct:pal("starting child node with ~s on host ~p for node ~p~n", [StartArgs, Addr, NodeName]),

    {ok, HostNode} = ct_slave:start(
        NodeName,
        [
            {kill_if_fail, true},
            {monitor_master, true},
            {erl_flags, StartArgs}
        ]
    ),

    ct:pal("\e[32m ---> Node ~p [OK] \e[0m", [HostNode]),

    pong = net_adm:ping(HostNode),

    Bn = #centauri_node{node = HostNode, name = NodeName, ipv4_address = IPv4Address},
    start_slaves(Config, T, [Bn | Acc]).

stop_slaves(Nodes) when is_list(Nodes) ->
    [stop_slave(Node) || Node <- Nodes].

stop_slave(Node) ->
    ct:pal("Stopping node ~p", [Node]),
    ct_slave:stop(Node#centauri_node.node),
    wait_for_stop(Node, 100).

halt_slave(Node) ->
    ct:pal("halting node ~p", [Node]),
    ct_rpc:call(Node#centauri_node.node, erlang, halt, []).

wait_for_stop(Node, 0) ->
    error({stop_failed_for, Node});
wait_for_stop(Node, Attempts) ->
    case ct_rpc:call(Node#centauri_node.node, erlang, node, []) of
        {badrpc, nodedown} ->
            ok;
        _ ->
            ct:sleep({seconds, 1}),
            wait_for_stop(Node, Attempts - 1)
    end.

start_application(Node, App) ->
    {ok, _} = ct_rpc:call(Node#centauri_node.node, application, ensure_all_started, [App]).

search_paths() ->
    Ld = code:lib_dir(),
    lists:filter(
        fun(P) -> string:prefix(P, Ld) =:= nomatch end,
        code:get_path()
    ).

data_dir(Config, NodeName) when is_atom(NodeName) ->
    filename:join([?config(priv_dir, Config), atom_to_list(NodeName)]).

make_dir(Dir) ->
    handle_ensure_dir(filelib:ensure_dir(Dir), Dir).

handle_ensure_dir(ok, Dir) ->
    handle_make_dir(file:make_dir(Dir)).

handle_make_dir(ok) ->
    ok;
handle_make_dir({error, eexist}) ->
    ok.

-spec poll_until(term(), integer() | infinity, timeout()) -> term() | false.
poll_until(Fun, 0, _PauseMS) ->
    Fun();
poll_until(Fun, Iterations, PauseMS) ->
    case Fun() of
        false ->
            ct:sleep(PauseMS),
            case Iterations of
                infinity -> poll_until(Fun, Iterations, PauseMS);
                Iterations -> poll_until(Fun, Iterations - 1, PauseMS)
            end;
        Reply ->
            Reply
    end.
