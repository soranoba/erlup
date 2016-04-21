%% @copyright 2016 Hinagiku Soranoba All Rights Reserved.

-module(erlup_utils_tests).
-include_lib("eunit/include/eunit.hrl").

%%----------------------------------------------------------------------------------------------------------------------
%% Unit Tests
%%----------------------------------------------------------------------------------------------------------------------

list_to_integer_if_possible_test_() ->
    [
     ?_assertEqual(1,     erlup_utils:list_to_integer_if_possible("1")),
     ?_assertEqual("a",   erlup_utils:list_to_integer_if_possible("a")),
     ?_assertEqual("1.2", erlup_utils:list_to_integer_if_possible("1.2"))
    ].

to_string_test_() ->
    [
     ?_assertEqual("1", erlup_utils:to_string("1")),
     ?_assertEqual("1", erlup_utils:to_string(<<"1">>))
    ].

absname_test_() ->
    [
     ?_assertEqual(filename:absname(""),
                   erlup_utils:absname("hoge/./../hoge/fugo/../../")),
     ?_assertEqual(filename:absname(<<"">>),
                   erlup_utils:absname(<<"hoge/./../hoge/fugo/../../">>))
    ].

base_dir_test_() ->
    [
     ?_assertEqual(filename:join(filename:absname(""), "myapp"),
                   erlup_utils:base_dir(filename:join("myapp", X)))
     || X <- [
              "releases/0.0.1/myapp.rel",
              "releases/0.0.1/relup",
              "releases/start_erl.data",
              "lib/mylib/ebin/mylib.appup",
              "lib/mylib/ebin/mymod.beam",
              "lib/mylib/ebin/mylib.app",
              "releases",
              "lib",
              "lib/mylib/ebin",
              "bin"
             ]
    ].

sort_vsns_test_() ->
    [
     ?_assertEqual(X, erlup_utils:sort_vsns(lists:reverse(X)))
     || X <- [
              ["1.8.7", "1.9", "1.9.0", "1.9.5", "1.10.4"],
              ["1.8.0-alpha", "1.8.0-beta", "v1.8.0", "1.8.1"],
              ["1.8.0-alpha.1", "1.8.0-alpha.2.0", "1.8.0-alpha.2.1", "1.8.0-alpha.2.beta"],
              ["1.7-alpha+build.1.2", "1.7-beta+build.0.1"]
             ]
    ].

sort_vsn2_test_() ->
    [
     fun() ->
             X = ["1.7-alpha+build.1.2", "1.7-alpha+build.0.1"],
             ?assertEqual(erlup_utils:sort_vsns(X),
                          lists:reverse(erlup_utils:sort_vsns(lists:reverse(X))))
     end
    ].
