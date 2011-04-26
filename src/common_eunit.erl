-module(common_eunit).

-export([test/1,
         test/2,
         test_generator/2]).


%% This is called Config in Common Test. It is the return type of the
%% init_per_* functions.
-type fixtures() :: [proplists:property()].

%% A representation of a test set that EUnit understands.
-type eu_test_rep() :: {{atom(), atom(), arity()}, fun()}
                     | {'setup', eu_setup(), eu_instantiator()}
                     | {'setup', eu_setup(), eu_cleanup(), eu_instantiator()}
                     | eu_control().

-type eu_setup()        :: fun(()           -> fixtures()).
-type eu_cleanup()      :: fun((fixtures()) -> _).
-type eu_instantiator() :: fun((fixtures()) -> eu_test_rep() | [eu_test_rep()]).

%% http://www.erlang.org/doc/apps/eunit/chapter.html#Control
-type eu_control() :: {atom(), eu_test_rep()}
                    | {atom(), term(), eu_test_rep()}.

%% This is an input type for us, which we transform into eu_control().
-type control() :: atom()             % E.g. 'sequence' or 'parallel'
                 | {atom(), term()}.  % E.g. {timetrap, {seconds, 30}}

%% Test case identification.
-type case_id() :: atom()            % Name of a test function
                 | {group, atom()}.  % Name of a test group


-spec test(atom()) -> 'ok' | 'error'.
test(Module) ->
    test(Module, Module:all()).

-spec test(atom(), case_id() | [case_id()]) -> 'ok' | 'error'.
test(Module, Cases) ->
    eunit:test(test_generator(test_module_name(Module), Cases)).

-spec test_generator(atom(), case_id() | [case_id()]) -> eu_test_rep().
test_generator(Module, Cases) ->
    Control = case erlang:function_exported(Module, suite, 0) of
        true  -> Module:suite();
        false -> []
    end,
    Instantiator0 = control_wrapper(Control, expand_cases(Module, Cases)),
    Instantiator1 = setup_wrapper(Module, Instantiator0,
                                  {init_per_suite, []}, {end_per_suite, []}),
    % Start with no fixtures, return the full test representation.
    Instantiator1([]).


-spec expand_cases(atom(), case_id() | [case_id()]) -> eu_instantiator().
expand_cases(Module, Cases) when is_list(Cases) ->
    Instantiators = [ expand_case(Module, Case) || Case <- Cases ],
    fun(Fixtures) -> [ F(Fixtures) || F <- Instantiators ] end;
expand_cases(Module, Cases) ->
    expand_cases(Module, [Cases]).

-spec expand_case(atom(), case_id()) -> eu_instantiator().
expand_case(Module, Case) when is_atom(Case) ->
    Instantiator = fun(Fixtures) ->
        % There are several ways to represent a test in EUnit. We use this:
        % {{M, F, A}, Fun/0}
        % It has the advantage that it lets EUnit give good feedback about
        % exactly which test function is being executed.
        {{Module, Case, 1}, fun() -> apply(Module, Case, [Fixtures]) end}
    end,
    setup_wrapper(Module, Instantiator,
                  {init_per_testcase, [Case]}, {end_per_testcase, [Case]});
expand_case(Module, {group, Group}) ->
    % We're dealing with a group of tests.
    {Control, Cases} = group_specification(Module, Group),
    Instantiator = control_wrapper(Control, expand_cases(Module, Cases)),
    setup_wrapper(Module, Instantiator,
                  {init_per_group, [Group]}, {end_per_group, [Group]}).

%% Control properties can be either one of these from Common Test:
%% - parallel
%% - sequence
%% - {timetrap, Time}
%% ...or any of http://www.erlang.org/doc/apps/eunit/chapter.html#Control
%%
-spec control_wrapper([control()], eu_instantiator()) -> eu_instantiator().
control_wrapper([Control|T], Instantiator0) ->
    Instantiator1 = control_wrapper(T, Instantiator0),
    case Control of
        parallel ->
            fun(Fixtures) ->
                {inparallel, Instantiator1(Fixtures)}
            end;
        sequence ->
            fun(Fixtures) ->
                {inorder, Instantiator1(Fixtures)}
            end;
        {timetrap, Time} ->
            Seconds = case Time of
                {hours, Hs}   -> Hs * 60 * 60;
                {minutes, Ms} -> Ms * 60;
                {seconds, Ss} -> Ss;
                MSs           -> MSs / 1000
            end,
            fun(Fixtures) ->
                {timeout, Seconds, Instantiator1(Fixtures)}
            end;
        {repeat, N} ->
            fun(Fixtures) ->
                [ Instantiator1(Fixtures) || _ <- lists:seq(1, N) ]
            end;
        C when is_atom(C) ->
            fun(Fixtures) ->
                {C, Instantiator1(Fixtures)}
            end;
        {C, Arg} ->
            fun(Fixtures) ->
                {C, Arg, Instantiator1(Fixtures)}
            end
    end;
control_wrapper([], Instantiator) ->
    Instantiator.

%% Maybe wrap a test fun in an EUnit setup tuple.
%% See http://www.erlang.org/doc/apps/eunit/chapter.html#Fixtures
%%
%% If we wrap and then exactly with what depend on which init_per_* and
%% end_per_* functions are exported by the module.
%%
-spec setup_wrapper(atom(), eu_instantiator(), Func, Func) -> eu_instantiator()
        when Func :: {atom(), list()}.
setup_wrapper(Module, Instantiator, {Setup, SA}, {Cleanup, CA}) ->
    case erlang:function_exported(Module, Setup, length(SA) + 1) of
        true ->
            case erlang:function_exported(Module, Cleanup, length(CA) + 1) of
                true ->
                    fun(Fixtures0) ->
                        {setup,
                            fun() ->
                                apply(Module, Setup, SA ++ [Fixtures0])
                            end,
                            fun(Fixtures1) ->
                                apply(Module, Cleanup, CA ++ [Fixtures1])
                            end,
                            Instantiator}
                    end;
                false ->
                    fun(Fixtures) ->
                        {setup,
                            fun() ->
                                apply(Module, Setup, SA ++ [Fixtures])
                            end,
                            Instantiator}
                    end
            end;
        false ->
            Instantiator
    end.

%% Calls Module:groups() and extracts the specification for the given group.
%%
%% Returns both a list of any group specific control properties as well as a
%% list of all the group's members (test cases and other groups).
%%
-spec group_specification(atom(), atom()) -> {[control()], [case_id()]}.
group_specification(Module, Group) ->
    case lists:keyfind(Group, 1, Module:groups()) of
        {_, Control, Cases} when is_list(Control), is_list(Cases) ->
            {Control, Cases};
        {_, Cases} when is_list(Cases) ->
            {[], Cases};
        false ->
            exit({missing_group, Group});
        _ ->
            exit({bad_group_spec, Group})
    end.

-spec test_module_name(atom()) -> atom().
test_module_name(Module) ->
    TestModuleSuffix = "_tests",
    case lists:suffix(TestModuleSuffix, atom_to_list(Module)) of
        true  -> Module;
        false -> list_to_atom(atom_to_list(Module) ++ TestModuleSuffix)
    end.
