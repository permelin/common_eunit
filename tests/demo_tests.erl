-module(demo_tests).

-include_lib("common_eunit/include/common_eunit.hrl").

suite() ->
    [{timetrap, {seconds, 5}}, % The entire suite cannot take longer.
     {node, slave1}].          % A slave node called slave1 is started
                               % on the same host, for the duration of
                               % the suite.

all() ->
    [alpha, {group, beta}, delta, epsilon].

groups() ->
    [{beta, [parallel, {repeat, 100}], [gamma]}].


%%% Setup and cleanup

init_per_suite(Config) ->
    % Set up fixtures here.
    Config.

end_per_suite(_Config) ->
    ok.

init_per_testcase(_Case, Config) ->
    % Set up fixtures here.
    Config.

end_per_testcase(_Case, _Config) ->
    ok.


%%% Tests

alpha(_Config) ->
    % Put test code here.
    ?assert(1 < 2).

gamma(_Config) ->
    % This test will run 100 times, in parallel. See groups/0.
    % Put test code here.
    ok.

delta() ->
    [{timetrap, 200}].
delta(_Config) ->
    % This test will abort if it takes longer than 200 ms.
    ok = timer:sleep(100).

epsilon() ->
    [{spawn, slave1}].
epsilon(Config) ->
    % This test will execute on the node slave1.
    Node = ?config(slave1, Config),
    ?assertEqual(node(), Node).
