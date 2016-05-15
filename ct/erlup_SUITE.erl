%% @copyright 2016 Hinagiku Soranoba All Rights Reserved.

-module(erlup_SUITE).
-include_lib("common_test/include/ct.hrl").
-include_lib("eunit/include/eunit.hrl").

-define(ALL_TESTS, [help, version, appup, relup]).

%%----------------------------------------------------------------------------------------------------------------------
%% 'Common Test' Callback Functions
%%----------------------------------------------------------------------------------------------------------------------

all() ->
    [
     {group, escript},
     {group, rebar3_plugin}
    ].

groups() ->
    [
     {escript,       [], ?ALL_TESTS ++ [vsn]},
     {rebar3_plugin, [], ?ALL_TESTS -- [version]}
    ].

init_per_suite(Config) ->
    os:cmd("cp -r " ++ ?config(data_dir, Config) ++ "spam_* " ++ ?config(priv_dir, Config) ++ "."),

    Spam1Dir = filename:join([?config(priv_dir, Config), "spam_v1"]),
    Spam2Dir = filename:join([?config(priv_dir, Config), "spam_v2"]),
    Spam3Dir = filename:join([?config(priv_dir, Config), "spam_v3"]),

    Erlup = case os:getenv("ERLUP") of
                ErlupPath when is_list(ErlupPath) -> {erlup, ErlupPath}
            end,
    Rebar = case os:getenv("REBAR") of
                RebarPath when is_list(RebarPath) -> {rebar, RebarPath}
            end,
    Dirs  = {dirs, [{1, Spam1Dir}, {2, Spam2Dir}, {3, Spam3Dir}]},

    lists:foreach(fun(Dir) ->
                          sh(filename:join(Dir, "_checkouts"),
                             "ln -s " ++ filename:join(?config(data_dir, Config), "../../../erlup") ++ " ."),
                          sh(Dir, RebarPath ++ " compile")
                  end, [Spam1Dir, Spam2Dir, Spam3Dir]),
    [Erlup, Rebar, Dirs | Config].

end_per_suite(Config) ->
    Config.

init_per_group(Group = escript, Config) ->
    [{group, Group},
     {command, ?config(erlup, Config) ++ " "},
     {d, lists:foldl(fun({_, Dir}, Acc) ->
                             Acc ++ " -d "
                                 ++ filename:join([Dir, "_build", "default", "rel", "spam"])
                     end, "", lists:reverse(?config(dirs, Config)))}
     | Config];
init_per_group(Group = rebar3_plugin, Config) ->
    [{group, Group},
     {command, ?config(rebar, Config) ++ " erlup "},
     {help, ?config(rebar, Config) ++ " help erlup "},
     {d, ""}
     | Config].

end_per_group(_, Config) ->
    Config.

init_per_testcase(TestCase, Config) ->
    case lists:member(TestCase, [help, version]) of
        true  -> ok;
        false ->
            Rebar            = ?config(rebar, Config),
            RecentVersionDir = ?config(3, ?config(dirs, Config)),
            lists:foreach(fun({_, Dir}) ->
                                  sh(Dir, Rebar ++ " release"),
                                  Rel = "/_build/default/rel",
                                  sh(RecentVersionDir ++ Rel, "cp -r " ++ Dir ++ Rel ++ "/* .")
                          end, ?config(dirs, Config))
    end,
    Config.

end_per_testcase(_, Config) ->
    lists:foreach(fun({_, Dir}) ->
                          sh(Dir, "rm -r _build/default/rel")
                  end, ?config(dirs, Config)),
    Config.

%%----------------------------------------------------------------------------------------------------------------------
%% Test Cases
%%----------------------------------------------------------------------------------------------------------------------

help(Config) ->
    Spam1Dir = ?config(1, ?config(dirs, Config)),

    {ok, State} = erlup:init(rebar_state:new()),
    Commands    = lists:filtermap(fun(P) ->
                                          case providers:namespace(P) of
                                              erlup -> {true, atom_to_list(providers:impl(P))};
                                              _     -> false
                                          end
                                  end, rebar_state:providers(State)),
    ct:log("commands : ~p", [Commands]),

    Fun = fun(Command) ->
                  HelpCommand1 = case ?config(group, Config) of
                                     escript       -> ?config(command, Config) ++ Command ++ " -h";
                                     rebar3_plugin -> ?config(help, Config) ++ Command
                                 end,
                  HelpCommand2 = HelpCommand1 ++ " 2>&1",

                  case find_lines(RetHelp = sh(Spam1Dir, HelpCommand2), "Usage") of
                      [Usage] ->
                          ct:log("$ ~s~n~s", [HelpCommand2, RetHelp]),
                          ?assertMatch({match, _}, re:run(Usage, Command));
                      [] ->
                          ct:log("$ ~s~n~s", [HelpCommand1, sh(Spam1Dir, HelpCommand1)]),
                          error(badhelp)
                  end
          end,
    lists:foreach(Fun, Commands).

version(Config) ->
    Spam1Dir = ?config(1, ?config(dirs, Config)),
    Command  = ?config(command, Config) ++ "-v 2>/dev/null",
    ct:log("$ ~s~n~s", [Command, Ret = sh(Spam1Dir, Command)]),
    ?assertMatch({match, _}, re:run(Ret, "erlup v[0-9]\.[0-9]\.[0-9].*")).

appup(Config) ->
    Res = sh(Dir = ?config(3, ?config(dirs, Config)),
             Command = ?config(command, Config) ++ "appup " ++ ?config(d, Config)),
    ct:log("$ ~s~n~s", [Command, Res]),
    Files = filelib:wildcard(filename:join([Dir, "_build", "default", "rel", "spam", "lib", "**/*.appup"])),
    ?assertEqual(3, length(Files)), % bbmustache, spam, spam_2
    lists:foreach(fun(F) ->
                          ExpectedAppup = filename:join(?config(data_dir, Config), filename:basename(F)),
                          {ok, GotTerms}      = file:consult(F),
                          {ok, ExpectedTerms} = file:consult(ExpectedAppup),
                          ct:log("$ cat ~s~n~p", [F, GotTerms]),
                          ?assertEqual(ExpectedTerms, GotTerms)
                  end, Files).

relup(Config) ->
    Res1 = sh(Dir = ?config(3, ?config(dirs, Config)),
             Command1 = ?config(command, Config) ++ "appup " ++ ?config(d, Config)),
    ct:log("$ ~s~n~s", [Command1, Res1]),
    Res2 = sh(Dir, Command2 = ?config(command, Config) ++ "relup " ++ ?config(d, Config)),
    ct:log("$ ~s~n~s", [Command2, Res2]),
    Files = filelib:wildcard(filename:join([Dir, "_build", "default", "rel", "spam", "releases", "**/relup"])),
    ?assertEqual(1, length(Files)).

vsn(Config) ->
    lists:foreach(fun({N, Dir0}) ->
                          Dir = filename:join([Dir0, "_build", "default", "rel", "spam"]),
                          Res = sh(Dir, Command = ?config(command, Config) ++ "vsn -d " ++ Dir),
                          ct:log("$ ~s~n~s", [Command, Res]),
                          ?assertMatch([_], find_lines(Res, "0\.0\." ++ integer_to_list(N)))
                  end, ?config(dirs, Config)).

%%----------------------------------------------------------------------------------------------------------------------
%% Internal Functions
%%----------------------------------------------------------------------------------------------------------------------

sh(Dir, Command) ->
    {ok, Cwd} = file:get_cwd(),
    ok  = filelib:ensure_dir(filename:join(Dir, "dummy")),
    ok  = file:set_cwd(Dir),
    Ret = os:cmd(Command),
    ok  = file:set_cwd(Cwd),
    Ret.

find_lines(Lines, MatchStr) ->
    find_lines(string:tokens(Lines, "\n"), MatchStr, []).

find_lines([], _MatchStr, Acc) ->
    lists:reverse(Acc);
find_lines([Line | Rest], MatchStr, Acc) ->
    case re:run(Line, MatchStr) of
        {match, _} -> find_lines(Rest, MatchStr, [Line | Acc]);
        nomatch    -> find_lines(Rest, MatchStr, Acc)
    end.
