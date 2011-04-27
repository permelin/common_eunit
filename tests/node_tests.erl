-module(node_tests).

-include_lib("common_eunit/include/common_eunit.hrl").

%%%
%%% Test spawning of slave nodes.
%%%

suite() ->
    [{node, slave1}].

all() ->
    [ping, {group, run_on_slave}].

groups() ->
    [{run_on_slave, [{node, slave2}, {spawn, slave2}], [on_slave]}].


init_per_suite(Config) ->
    ?assert(pong == net_adm:ping(?config(slave1, Config))),
    [{master, node()}|Config].

%%% Tests

ping(Config) ->
    ?assert(node() == ?config(master, Config)),
    ?assert(pong == net_adm:ping(?config(slave1, Config))).

on_slave(Config) ->
    ?assert(node() == ?config(slave2, Config)),
    ?assert(pong == net_adm:ping(?config(master, Config))).
