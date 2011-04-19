-module(misc_tests).

-include_lib("common_eunit/include/common_eunit.hrl").

%%%
%%% Primarily tests groups, nested groups and special control instructions.
%%%

suite() ->
    [{inparallel, 8}, {timeout, 1}].

all() ->
    [simple,
     {group, main},
     {group, order},
     {group, many},
     {group, empty}].

groups() ->
    [{main, [simple, {group, nested}]},
     {nested, [simple]},
     {order, [inorder], [{group, sequential}, {group, parallel}]},
     {sequential, [ singleton || _ <- lists:seq(1, 20) ]},
     {parallel, [inparallel, {timeout, 0.2}], [ slow || _ <- lists:seq(1, 10) ]},
     {many, [ many || _ <- lists:seq(1, 100) ]},
     {empty, [], []}].


%%% Setup and cleanup

init_per_suite(Config) ->
    ets:new(test_state, [named_table, public]),
    ets:insert(test_state, [{many, 0}, {seq, 0}, {simple, 0}, {slow, 0}]),
    Config.

end_per_suite(_Config) ->
    ?assertEqual([{many, 100}, {seq, 20}, {simple, 3}, {slow, 10}],
                 lists:sort(ets:tab2list(test_state))).


%%% Tests

simple(_Conf) ->
    ets:update_counter(test_state, simple, 1).

many(_Conf) ->
    ets:update_counter(test_state, many, 1).

slow(_Conf) ->
    ets:update_counter(test_state, slow, 1),
    timer:sleep(100).

singleton(_Conf) ->
    ets:update_counter(test_state, seq, 1),
    % Test that this case is never called in parallel.
    ?assert(ets:insert_new(test_state, {lock, true})),
    timer:sleep(10),
    ets:delete(test_state, lock).

