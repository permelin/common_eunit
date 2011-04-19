-module(common_eunit).

-export([test/1,
         test/2,
         test_generator/2]).

-type test_name() :: atom()
                   | {group, atom()}.

-type test_rep()  :: {{module(), atom(), arity()}, fun()} 
                   | {setup, fun(), fun()}
                   | {setup, fun(), fun(), fun()}.

-type config()    :: [proplists:property()].

-type control()   :: tuple()
                   | atom().


-spec test(module()) -> 'ok' | 'error'.
test(Module) ->
    test(Module, Module:all()).

-spec test(module(), [test_name()]) -> 'ok' | 'error'.
test(Module, Cases) ->
    eunit:test(test_generator(test_module_name(Module), Cases)).

-spec test_generator(module(), [test_name()]) -> test_rep().
test_generator(Module, Cases) ->
    Control = case erlang:function_exported(Module, suite, 0) of
        true  -> Module:suite();
        false -> []
    end,
    % EUnit test representation of all the cases.
    SuiteRep = fun(Config) ->
        TestReps = expand_cases(Module, Cases, Config),
        control_wrapper(Control, TestReps)
    end,
    % Wrap the suite with setup and cleanup functions.
    % Note that init_per_suite/1 starts with an empty config.
    Setup   = fun()       -> Module:init_per_suite([]) end,
    Cleanup = fun(Config) -> Module:end_per_suite(Config) end,
    setup_wrapper(Module, [], SuiteRep, {Setup, init_per_suite, 1},
                                        {Cleanup, end_per_suite, 1}).

-spec expand_cases(module(), [test_name()], config()) -> [test_rep()].
expand_cases(Module, Cases, Config) ->
    if is_list(Cases) ->
            [ expand_case(Module, Case, Config) || Case <- Cases ];
       is_atom(Cases) ->
           expand_cases(Module, [Cases], Config)
    end.

-spec expand_case(module(), test_name(), config()) -> test_rep().
expand_case(Module, {group, GroupName}, Config0) ->
    % We're dealing with a group of tests.
    {Control, Cases} = group_specification(Module, GroupName),
    % The group's test representation.
    GroupRep = fun(Config1) ->
        TestReps = expand_cases(Module, Cases, Config1),
        control_wrapper(Control, TestReps)
    end,
    % Setup and cleanup. Config is passed on down from suite, and any
    % parent group, setup.
    Setup   = fun()        -> Module:init_per_group(GroupName, Config0) end,
    Cleanup = fun(Config1) -> Module:end_per_group(GroupName, Config1) end,
    setup_wrapper(Module, Config0, GroupRep, {Setup, init_per_group, 2},
                                             {Cleanup, end_per_group, 2});
expand_case(Module, CaseName, Config0) ->
    Test = fun(Config1) ->
        % There are several ways to represent a test in EUnit. We use this:
        % {{M, F, A}, Fun/0}
        % It has the advantage that it lets EUnit give good feedback about
        % exactly which test function is being executed.
        {{Module, CaseName, 1}, fun() -> apply(Module, CaseName, [Config1]) end}
    end,
    % Setup and cleanup. Config is passed on down from suite and group setup.
    Setup = fun() ->
        Module:init_per_testcase(CaseName, Config0)
    end,
    Cleanup = fun(Config1) ->
        Module:end_per_testcase(CaseName, Config1)
    end,
    setup_wrapper(Module, Config0, Test, {Setup, init_per_testcase, 2},
                                         {Cleanup, end_per_testcase, 2}).

%% Control properties can be either one of these from Common Test:
%% - parallel
%% - sequence
%% - {timetrap, Time}
%% ...or any of http://www.erlang.org/doc/apps/eunit/chapter.html#Control
%%
-spec control_wrapper([control()], [test_rep()]) -> test_rep() | [test_rep()].
control_wrapper([parallel|T], Tests) ->
    {inparallel, control_wrapper(T, Tests)};
control_wrapper([sequence|T], Tests) ->
    {inorder, control_wrapper(T, Tests)};
control_wrapper([{timetrap, Time}|T], Tests) ->
    Seconds = case Time of
        {hours, Hs}   -> Hs * 60 * 60;
        {minutes, Ms} -> Ms * 60;
        {seconds, Ss} -> Ss;
        MSs           -> MSs / 1000
    end,
    {timeout, Seconds, control_wrapper(T, Tests)};
control_wrapper([Control|T], Tests) when is_tuple(Control) ->
    % {inparallel, 5} is turned into {inparallel, 5, Tests}
    list_to_tuple(tuple_to_list(Control) ++ [control_wrapper(T, Tests)]);
control_wrapper([Control|T], Tests) when is_atom(Control) ->
    % 'inparallel' is turned into {inparallel, Tests}
    {Control, control_wrapper(T, Tests)};
control_wrapper([], Tests) ->
    Tests.

%% Maybe wrap a test fun in an EUnit setup tuple. If we wrap it and exactly
%% with what depend on if the module exports the right init_per_* and
%% end_per_* functions.
%%
-spec setup_wrapper(module(), config(), fun(), maybe(), maybe()) -> test_rep().
-type maybe() :: {fun(), atom(), arity()}.
setup_wrapper(Module, Config, Tests, {SF, SE, SA}, {CF, CE, CA}) ->
    case erlang:function_exported(Module, SE, SA) of
        true ->
            case erlang:function_exported(Module, CE, CA) of
                true ->
                    {setup, SF, CF, Tests};
                false ->
                    {setup, SF, Tests}
            end;
        false ->
            Tests(Config)
    end.

%% Calls Module:groups() and extracts the specification for the given group.
%%
%% Returns both a list of any group specific control properties as well as a
%% list of all the group's members (test cases and other groups).
%%
-spec group_specification(module(), atom()) -> {[control()], [test_name()]}.
group_specification(Module, GroupName) ->
    case lists:keyfind(GroupName, 1, Module:groups()) of
        {_, Control, Cases} when is_list(Control), is_list(Cases) ->
            {Control, Cases};
        {_, Cases} when is_list(Cases) ->
            {[], Cases};
        false ->
            exit({missing_group, GroupName});
        _ ->
            exit({bad_group_spec, GroupName})
    end.

-spec test_module_name(module()) -> module().
test_module_name(Module) ->
    TestModuleSuffix = "_tests",
    case lists:suffix(TestModuleSuffix, atom_to_list(Module)) of
        true  -> Module;
        false -> list_to_atom(atom_to_list(Module) ++ TestModuleSuffix)
    end.

