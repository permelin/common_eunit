-module(setup_tests).

-include_lib("common_eunit/include/common_eunit.hrl").

%%%
%%% Check that all init_per_* and end_per_* are called.
%%%

all() ->
    [check_case, {group, check}].

groups() ->
    [{check, [check_group]}].


%%% Setup and cleanup

init_per_suite(Config) ->
    [suite|Config].

end_per_suite(Config) ->
    ?assertEqual([suite], Config).

init_per_group(Group, Config) ->
    [{group, Group}|Config].

end_per_group(Group, Config) ->
    ?assertEqual([{group, Group}, suite], Config).

init_per_testcase(Case, Config) ->
    [{test, Case}|Config].

end_per_testcase(Case, Config) ->
    ?assertEqual(Case, ?config(test, Config)),
    ?assertEqual(true, ?config(suite, Config)).


%%% Tests

check_case(Config) ->
    ?assertEqual([{test, check_case}, suite], Config).

check_group(Config) ->
    ?assertMatch([{test, check_group}, {group, check}, suite], Config).
