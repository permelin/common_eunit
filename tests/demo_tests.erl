-module(demo_tests).

-include_lib("common_eunit/include/common_eunit.hrl").

suite() ->
    [{timetrap, {seconds, 5}}].

all() ->
    [lookup_missing, insert, {group, atomic}].

groups() ->
    [{atomic, [parallel, {repeat, 50}], [update_counter]}].

%%% Setup and cleanup

init_per_suite(Config) ->
    Table = ets:new(test_state, [public]),
    [{table, Table}|Config].

end_per_suite(Config) ->
    Table = ?config(table, Config),
    ets:delete(Table),
    ok.

init_per_group(atomic, Config) ->
    Table = ?config(table, Config),
    ets:insert(Table, [{counter, 0}]),
    Config;
init_per_group(_Group, Config) ->
    Config.

init_per_testcase(Case, Config) when Case /= update_counter ->
    Table = ?config(table, Config),
    ets:delete_all_objects(Table),
    Config;
init_per_testcase(_Case, Config) ->
    Config.

%%% Tests

lookup_missing(Config) ->
    Table = ?config(table, Config),
    ?assertEqual([], ets:lookup(Table, foo)).

insert(Config) ->
    Table = ?config(table, Config),
    % insert
    ets:insert(Table, {foo, bar}),
    ?assertEqual([{foo, bar}], ets:lookup(Table, foo)),
    % insert_new
    true = ets:insert_new(Table, {bar, foo}),
    ?assertEqual([{bar, foo}], ets:lookup(Table, bar)).

update_counter(Config) ->
    Table = ?config(table, Config),
    V1 = ets:update_counter(Table, counter, 1),
    V2 = ets:lookup_element(Table, counter, 2),
    ?assert(is_integer(V1)),
    ?assert(is_integer(V2)),
    ?assert(V2 >= V1).

