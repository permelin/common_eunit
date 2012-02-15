EUnit masquerading as Common Test
=================================

Common EUnit lets you write test suites for EUnit that looks like suites for Common Test.

Why would you want to do that? Because when you do system and integration testing you usually need to do a lot of setup and teardown of fixtures, which can otherwise be cumbersome in EUnit. Other than that, EUnit is very capable even when you move beyond unit testing.

EUnit has the concept of tests as data. You can hand it nested representations of sets of tests. Common EUnit is a test generator that converts a test suite module into such a representation and hands it over to EUnit to execute. No parse transforms or other magic involved.

Example
-------

The example below can be found in `tests/demo_tests.erl`. Its structure should look very familiar to anyone who has used Common Test. It can be started like this:

    $ erl -pz $(pwd)/ebin -sname tester
    Erlang R14B02 (erts-5.8.3) [source] [smp:2:2] [rq:2] [async-threads:0] [hipe] [kernel-poll:false]

    Eshell V5.8.3  (abort with ^G)
    1> make:all([load]).
    Recompile: src/common_eunit
    ...
    up_to_date
    2> eunit:test(demo_tests).
      All 103 tests passed.

**Please note that we simply invoked EUnit here.** If you include the `common_eunit.hrl` header you can then run EUnit as usual.

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
* `Testcase/0`

Groups
------
Groups can be nested to the extent that a group can call on other groups.

    groups() -> [{g1, [], [case1, case2]},
                 {g2, [], [case3, {group, g1}]}].

But group definitions cannot be nested.

    % Will not work
    groups() -> [{g1, [], [{g2, [], [case1, case2]}]}].

Test execution properties
--------------------------
These properties can be given either from `suite/0`, in the group definition or
from `Testcase/0`:

* `{timetrap, Time}`
* `parallel`
* `{parallel, N}` where N is max number of concurrent processes
* `inorder`
* `{repeat, N}`
* `{node, Name}` uses `slave:start_link/3` to start a node on the local host
* `{spawn, Name}` runs the tests on slave node Name

What is missing
---------------
* Shuffled tests
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
    ...
    up_to_date
    2> eunit:test({dir, "ebin"}).
      All 326 tests passed.
    
If you want to run a single test case instead of the full suite you have to call common_eunit directly:

    2> common_eunit:test(demo_tests, [alpha]).
      Test passed.

Run a group:

    3> common_eunit:test(demo_tests, [{group, beta}]).
      All 100 tests passed.

