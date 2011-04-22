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
    SuiteFun = setup_wrapper(Module, TestFun, {init_per_suite, []},
                                              {end_per_suite, []}),
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
    setup_wrapper(Module, TestFun, {init_per_testcase, [CaseName]},
                                   {end_per_testcase, [CaseName]});
expand_case(Module, {group, GroupName}) ->
    % We're dealing with a group of tests.
    {Control, Cases} = group_specification(Module, GroupName),
    TestFun = control_wrapper(Control, expand_cases(Module, Cases)),
    setup_wrapper(Module, TestFun, {init_per_group, [GroupName]},
                                   {end_per_group, [GroupName]}).

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

%% Maybe wrap a test fun in an EUnit setup tuple.
%% See http://www.erlang.org/doc/apps/eunit/chapter.html#Fixtures
%%
%% If we wrap and then exactly with what depend on which init_per_* and
%% end_per_* functions are exported by the module.
%%
-spec setup_wrapper(module(), test_fun(), Callback, Callback) -> test_fun()
        when Callback :: {atom(), list()}.
setup_wrapper(Module, TestFun, {Setup, SA}, {Cleanup, CA}) ->
    case erlang:function_exported(Module, Setup, length(SA) + 1) of
        true ->
            case erlang:function_exported(Module, Cleanup, length(CA) + 1) of
                true ->
                    fun(Config0) ->
                        {setup,
                            fun() ->
                                apply(Module, Setup, SA ++ [Config0])
                            end,
                            fun(Config1) ->
                                apply(Module, Cleanup, CA ++ [Config1])
                            end,
                            TestFun}
                    end;
                false ->
                    fun(Config) ->
                        {setup,
                            fun() ->
                                apply(Module, Setup, SA ++ [Config])
                            end,
                            TestFun}
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
