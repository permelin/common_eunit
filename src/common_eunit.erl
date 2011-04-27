-module(common_eunit).

-export([test/1,
         test/2,
         test_generator/2]).

%% This is called Config in Common Test. It is the return type of the
%% init_per_* functions.
-type fixtures() :: [proplists:property()].

%% A representation of a test set that EUnit understands.
-type eu_test_rep() :: {{atom(), atom(), arity()}, fun()}
                     | {'setup', eu_setup(), eu_instant()}
                     | {'setup', eu_setup(), eu_cleanup(), eu_instant()}
                     | eu_control().

%% http://www.erlang.org/doc/apps/eunit/chapter.html#Fixtures
-type eu_setup()    :: fun(()           -> fixtures()).
-type eu_cleanup()  :: fun((fixtures()) -> _).

%% EUnit test instantiator.
-type eu_instant()  :: fun((fixtures()) -> eu_test_rep() | [eu_test_rep()]).

%% http://www.erlang.org/doc/apps/eunit/chapter.html#Control
-type eu_control() :: {atom(), eu_test_rep()}
                    | {atom(), term(), eu_test_rep()}.

%% This is an input type for us, which we transform into eu_control().
-type prop() :: atom()             % E.g. 'parallel'
              | {atom(), term()}.  % E.g. {timetrap, {seconds, 30}}

%% Test case identification.
-type case_id() :: atom()            % Name of a test function
                 | {group, atom()}.  % Name of a test group

-type level() :: 'suite' | 'group' | 'testcase'.


%%%
%%% Interface
%%%

-spec test(atom()) -> 'ok' | 'error'.
test(Module) ->
    test(Module, Module:all()).

-spec test(atom(), [case_id()]) -> 'ok' | 'error'.
test(Module, Cases) ->
    eunit:test(test_generator(test_module_name(Module), Cases)).

-spec test_generator(atom(), [case_id()]) -> eu_test_rep().
test_generator(Module, Cases) ->
    Props = case erlang:function_exported(Module, suite, 0) of
        true  -> Module:suite();
        false -> []
    end,
    Instant0 = expand_cases(Module, Cases),
    Instant1 = wrap(Module, Instant0, Props, suite, []),
    % Start with no fixtures, return the full test representation.
    Instant1([]).


%%%
%%% Implementation
%%%

-spec fixture_callbacks(level()) -> {atom(), atom()}.
fixture_callbacks(suite)    -> {init_per_suite, end_per_suite};
fixture_callbacks(group)    -> {init_per_group, end_per_group};
fixture_callbacks(testcase) -> {init_per_testcase, end_per_testcase}.

%% Valid test execution properties, in the order they should be applied.
%%
%% The first group is applied before the init_per_ call, the second group
%% between init_per_ and the underlying test cases.
%%
-spec props(level()) -> {[atom()], [atom()]}.
props(suite)    -> {[timetrap], [parallel, inorder, repeat]};
props(group)    -> {[timetrap], [parallel, inorder, repeat]};
props(testcase) -> {[timetrap], [parallel, inorder, repeat]}.

%% Add test properties and calls to the appropriate init_per_ and end_per_.
%%
-spec wrap(atom(), eu_instant(), [prop()], level(), list())-> eu_instant().
wrap(Module, Instant0, Props, Level, Args) ->
    {InitPer, EndPer} = fixture_callbacks(Level),
    {PreProps, PostProps} = partition_props(Props, Level),
    Instant1 = add_props(PostProps, Instant0),
    Instant2 = add_setup(Module, Instant1, InitPer, EndPer, Args),
    add_props(PreProps, Instant2).

%% Recurse down through groups and test cases and return fully wrapped
%% instantiators.
%%
-spec expand_cases(atom(), [case_id()]) -> eu_instant().
expand_cases(Module, Cases) ->
    Instants = [ expand_case(Module, Case) || Case <- Cases ],
    fun(Fixtures) -> [ F(Fixtures) || F <- Instants ] end.

-spec expand_case(atom(), case_id()) -> eu_instant().
expand_case(Module, Case) when is_atom(Case) ->
    Props = case erlang:function_exported(Module, Case, 0) of
        true  -> Module:Case();
        false -> []
    end,
    Instant = fun(Fixtures) ->
        % There are several ways to represent a test case to EUnit. We use:
        % {{M, F, A}, Fun/0}
        % It has the advantage that it lets EUnit give good feedback about
        % exactly which test function is being executed.
        {{Module, Case, 1}, fun() -> apply(Module, Case, [Fixtures]) end}
    end,
    wrap(Module, Instant, Props, testcase, [Case]);
expand_case(Module, {group, Group}) ->
    {Props, Cases} = group_specification(Module, Group),
    Instant = expand_cases(Module, Cases),
    wrap(Module, Instant, Props, group, [Group]).

%% See props/1 and http://www.erlang.org/doc/apps/eunit/chapter.html#Control
%%
-spec add_props([prop()], eu_instant()) -> eu_instant().
add_props([Prop|T], Instant0) ->
    Instant1 = add_props(T, Instant0),
    case Prop of
        parallel ->
            fun(Fixtures) ->
                {inparallel, Instant1(Fixtures)}
            end;
        {parallel, N} ->
            fun(Fixtures) ->
                {inparallel, N, Instant1(Fixtures)}
            end;
        inorder ->
            fun(Fixtures) ->
                {inorder, Instant1(Fixtures)}
            end;
        {timetrap, Time} ->
            Seconds = case Time of
                {hours, Hs}   -> Hs * 60 * 60;
                {minutes, Ms} -> Ms * 60;
                {seconds, Ss} -> Ss;
                MSs           -> MSs / 1000
            end,
            fun(Fixtures) ->
                {timeout, Seconds, Instant1(Fixtures)}
            end;
        {repeat, N} ->
            fun(Fixtures) ->
                [ Instant1(Fixtures) || _ <- lists:seq(1, N) ]
            end
    end;
add_props([], Instant) ->
    Instant.

%% Partition a list of execution properties into two groups to be applied
%% before and after the appropriate init_per_ callback. Also orders the
%% individual properties correctly.
%%
-spec partition_props([prop()], level()) -> {[prop()], [prop()]}.
partition_props(Props, Level) ->
    {PreKeys, PostKeys} = props(Level),
    Unfolded = proplists:unfold(Props),
    Pre = filter_props(PreKeys, Unfolded),
    Post = filter_props(PostKeys, Unfolded),
    case Unfolded -- (Pre ++ Post) of
        [] ->
            {proplists:compact(Pre), proplists:compact(Post)};
        [{Illegal, _}|_] ->
            exit({illegal_property, Illegal})
    end.

-spec filter_props([atom()], [{atom(), term()}]) -> [{atom(), term()}].
filter_props([Key|T], Props) ->
    case lists:keysearch(Key, 1, Props) of
        {value, P} -> [P|filter_props(T, Props)];
        false      -> filter_props(T, Props)
    end;
filter_props([], _) ->
    [].

%% Wrap an instantiator in an EUnit setup tuple with callbacks to init_per_ and
%% end_per_ functions.
%% See http://www.erlang.org/doc/apps/eunit/chapter.html#Fixtures
%%
-spec add_setup(atom(), eu_instant(), atom(), atom(), list()) -> eu_instant().
add_setup(Module, Instant, InitPer, EndPer, Args) ->
    Arity = length(Args) + 1,
    case erlang:function_exported(Module, InitPer, Arity) of
        true ->
            case erlang:function_exported(Module, EndPer, Arity) of
                true ->
                    fun(Fixtures0) ->
                        {setup,
                            fun() ->
                                apply(Module, InitPer, Args ++ [Fixtures0])
                            end,
                            fun(Fixtures1) ->
                                apply(Module, EndPer, Args ++ [Fixtures1])
                            end,
                            Instant}
                    end;
                false ->
                    fun(Fixtures) ->
                        {setup,
                            fun() ->
                                apply(Module, InitPer, Args ++ [Fixtures])
                            end,
                            Instant}
                    end
            end;
        false ->
            Instant
    end.

%% Call Module:groups() and extract the specification for the given group.
%%
%% Return both a list of any group specific test properties as well as a
%% list of all the group's members (test cases and other groups).
%%
-spec group_specification(atom(), atom()) -> {[prop()], [case_id()]}.
group_specification(Module, Group) ->
    case lists:keyfind(Group, 1, Module:groups()) of
        {_, Props, Cases} when is_list(Props), is_list(Cases) ->
            {Props, Cases};
        {_, Cases} when is_list(Cases) ->
            {[], Cases};
        false ->
            exit({missing_group, Group});
        _ ->
            exit({bad_group_spec, Group})
    end.

%% Add a _tests suffix to the module name if missing.
%%
-spec test_module_name(atom()) -> atom().
test_module_name(Module) ->
    TestModuleSuffix = "_tests",
    case lists:suffix(TestModuleSuffix, atom_to_list(Module)) of
        true  -> Module;
        false -> list_to_atom(atom_to_list(Module) ++ TestModuleSuffix)
    end.
