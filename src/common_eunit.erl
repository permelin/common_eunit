-module(common_eunit).

-export([test/1,
         test/2,
         test_generator/2]).

-type test_name() :: atom()
                   | {group, atom()}.

-type test_rep()  :: {{module(), atom(), arity()}, fun()} 
                   | {'setup', fun(), fun()}
                   | {'setup', fun(), fun(), fun()}
                   | {atom(), test_rep()}
                   | {atom(), term(), test_rep()}.

-type config()    :: [proplists:property()].

-type control()   :: tuple()
                   | atom().

-type test_fun()  :: fun((config()) -> test_rep()).


-spec test(module()) -> 'ok' | 'error'.
test(Module) ->
    test(Module, Module:all()).

-spec test(module(), test_name() | [test_name()]) -> 'ok' | 'error'.
test(Module, Cases) ->
    eunit:test(test_generator(test_module_name(Module), Cases)).

-spec test_generator(module(), test_name() | [test_name()]) -> test_rep().
test_generator(Module, Cases) ->
    Control = case erlang:function_exported(Module, suite, 0) of
        true  -> Module:suite();
        false -> []
    end,
    TestFun = control_wrapper(Control, expand_cases(Module, Cases)),
    Setup   = fun(Config) -> Module:init_per_suite(Config) end,
    Cleanup = fun(Config) -> Module:end_per_suite(Config) end,
    SuiteFun = setup_wrapper(Module, TestFun, {Setup, init_per_suite, 1},
                                              {Cleanup, end_per_suite, 1}),
    % Start with an empty config, return the full test representation.
    SuiteFun([]).

-spec expand_cases(module(), test_name() | [test_name()]) -> test_fun().
expand_cases(Module, Cases) ->
    if is_list(Cases) ->
            TestFuns = [ expand_case(Module, Case) || Case <- Cases ],
            fun(Config) -> [ F(Config) || F <- TestFuns ] end;
       is_atom(Cases); is_tuple(Cases) ->
           expand_cases(Module, [Cases])
    end.

-spec expand_case(module(), test_name()) -> test_fun().
expand_case(Module, CaseName) when is_atom(CaseName) ->
    TestFun = fun(Config) ->
        % There are several ways to represent a test in EUnit. We use this:
        % {{M, F, A}, Fun/0}
        % It has the advantage that it lets EUnit give good feedback about
        % exactly which test function is being executed.
        {{Module, CaseName, 1}, fun() -> apply(Module, CaseName, [Config]) end}
    end,
    Setup   = fun(Config) -> Module:init_per_testcase(CaseName, Config) end,
    Cleanup = fun(Config) -> Module:end_per_testcase(CaseName, Config) end,
    setup_wrapper(Module, TestFun, {Setup, init_per_testcase, 2},
                                   {Cleanup, end_per_testcase, 2});
expand_case(Module, {group, GroupName}) ->
    % We're dealing with a group of tests.
    {Control, Cases} = group_specification(Module, GroupName),
    TestFun = control_wrapper(Control, expand_cases(Module, Cases)),
    Setup   = fun(Config) -> Module:init_per_group(GroupName, Config) end,
    Cleanup = fun(Config) -> Module:end_per_group(GroupName, Config) end,
    setup_wrapper(Module, TestFun, {Setup, init_per_group, 2},
                                   {Cleanup, end_per_group, 2}).

%% Control properties can be either one of these from Common Test:
%% - parallel
%% - sequence
%% - {timetrap, Time}
%% ...or any of http://www.erlang.org/doc/apps/eunit/chapter.html#Control
%%
-spec control_wrapper([control()], test_fun()) -> test_fun().
control_wrapper([Control|T], TestFun0) ->
    TestFun1 = control_wrapper(T, TestFun0),
    fun(Config) ->
        case Control of
            parallel ->
                {inparallel, TestFun1(Config)};
            sequence ->
                {inorder, TestFun1(Config)};
            {timetrap, Time} ->
                Seconds = case Time of
                    {hours, Hs}   -> Hs * 60 * 60;
                    {minutes, Ms} -> Ms * 60;
                    {seconds, Ss} -> Ss;
                    MSs           -> MSs / 1000
                end,
                {timeout, Seconds, TestFun1(Config)};
            C when is_atom(C) ->
                {C, TestFun1(Config)};
            {C, Arg} ->
                {C, Arg, TestFun1(Config)}
        end
    end;
control_wrapper([], TestFun) ->
    TestFun.

%% Maybe wrap a test fun in an EUnit setup tuple. If we wrap it and exactly
%% with what depend on if the module exports the right init_per_* and
%% end_per_* functions.
%%
-spec setup_wrapper(module(), test_fun(), setup(), cleanup()) -> test_fun().
-type setup()   :: {fun((config()) -> config()), atom(), arity()}.
-type cleanup() :: {fun((config()) -> _),        atom(), arity()}.
setup_wrapper(Module, TestFun, {SF, SE, SA}, {CF, CE, CA}) ->
    case erlang:function_exported(Module, SE, SA) of
        true ->
            case erlang:function_exported(Module, CE, CA) of
                true ->
                    fun(Config) ->
                        {setup, fun() -> SF(Config) end, CF, TestFun}
                    end;
                false ->
                    fun(Config) ->
                        {setup, fun() -> SF(Config) end, TestFun}
                    end
            end;
        false ->
            TestFun
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
