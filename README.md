EUnit masquerading as Common Test
=================================

Common EUnit lets you write test suites for EUnit that looks like suites for Common Test.

Why would you want to do that? Because when you do system and integration testing you usually need to do a lot of setup and teardown, which can otherwise be cumbersome in EUnit. Other than that, EUnit is very capable even when you move beyond unit testing.

EUnit has the concept of tests as data. You can hand it nested representations of sets of tests. Common EUnit is a test generator that converts a test suite module into such a representation and hands it over to EUnit to execute.

Example
-------

The example below can be found in `tests/demo_tests.erl` and can be started like this:

    $ erl -pz $(pwd)/ebin
    Erlang R14B02 (erts-5.8.3) [source] [smp:2:2] [rq:2] [async-threads:0] [hipe] [kernel-poll:false]

    Eshell V5.8.3  (abort with ^G)
    1> eunit:test(demo_tests).
      All 52 tests passed.

Please note that we simply invoked EUnit here. If you include the `common_eunit.hrl` header you can then run EUnit as usual.

    -module(demo_tests).
    
    -include_lib("common_eunit/include/common_eunit.hrl").
    
    suite() ->
        [{timetrap, {seconds, 5}}].
    
    all() ->
        [lookup_missing, insert, {group, atomic}].
    
    groups() ->
        % We don't have Common Test's repeating groups feature, so we have to
        % fake it with a list comprehension.
        [{atomic, [parallel], [ update_counter || _ <- lists:seq(1, 50) ]}].
    
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

Callbacks
---------
All the Common Test callbacks are supported.

* `suite/0`
* `all/0`
* `groups/0`
* `init_per_suite/1`
* `end_per_suite/1`
* `init_per_group/2`
* `end_per_group/2`
* `init_per_testcase/2`
* `end_per_testcase/2`

Groups
------
Groups can be nested to the extent that a group can call on other groups.

    groups() -> [{g1, [], [case1, case2]},
                 {g2, [], [case3, {group, g1}]}].

But group definitions cannot be nested.

    % Will not work
    groups() -> [{g1, [], [{g2, [], [case1, case2]}]}].

Suite and group properties
--------------------------
These properties can be given either from `suite/0` or in the group definition:

* `{timetrap, Time}`
* `parallel`
* `sequence`

Starting nodes
--------------
Where you would call `test_server:start_node/3` with Common Test you can often get away with `slave:start_link/3` instead.

What is missing
---------------
* Shuffled tests
* Repeating groups
* `data_dir`
* `priv_dir`
* Many, many other things

Usage
-----
As long as your test suite includes this header and you have `common_eunit/ebin` in your code path you should be able to use EUnit as usual.

    -include_lib("common_eunit/include/common_eunit.hrl").

For example, running all included tests:

    $ erl -pz $(pwd)/ebin
    Erlang R14B02 (erts-5.8.3) [source] [smp:2:2] [rq:2] [async-threads:0] [hipe] [kernel-poll:false]
    
    Eshell V5.8.3  (abort with ^G)
    1> make:all([load]).
    Recompile: src/common_eunit
    Recompile: tests/setup_tests
    Recompile: tests/misc_tests
    Recompile: tests/minimal_tests
    Recompile: tests/demo_tests
    up_to_date
    2> eunit:test({dir, "ebin"}).
      All 188 tests passed.
    
If you want to run a single test case instead of the full suite you have to call common_eunit directly:

    2> common_eunit:test(demo_tests, [insert]).
      Test passed.

Run a group:

    3> common_eunit:test(demo_tests, [{group, atomic}]).
      All 50 tests passed.

