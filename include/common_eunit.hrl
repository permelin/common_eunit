-include_lib("eunit/include/eunit.hrl").

-compile(export_all).

-define(config(Key, Config), proplists:get_value(Key, Config)).

-ifndef(NOTEST).

run_all_eunit_tests_with_common_eunit_test_() ->
    common_eunit:test_generator(?MODULE, all()).

-endif.

