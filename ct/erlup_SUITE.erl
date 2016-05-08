%% @copyright 2016 Hinagiku Soranoba All Rights Reserved.

-module(erlup_SUITE).
-include_lib("common_test/include/ct.hrl").
-include_lib("eunit/include/eunit.hrl").

-define(ALL_TESTS, [help, version]).

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
     {escript,       [], ?ALL_TESTS},
     {rebar3_plugin, [], ?ALL_TESTS}
    ].

init_per_suite(Config) ->
    os:cmd("cp -r " ++ ?config(data_dir, Config) ++ "* " ++ ?config(priv_dir, Config) ++ "."),

    Spam1Dir = filename:join([?config(priv_dir, Config), "spam_v1", "_checkouts"]),
    Spam2Dir = filename:join([?config(priv_dir, Config), "spam_v2", "_checkouts"]),
    sh(Spam1Dir, "ln -s " ++ filename:join(?config(data_dir, Config), "../../../erlup") ++ " ."),
    sh(Spam2Dir, "ln -s " ++ filename:join(?config(data_dir, Config), "../../../erlup") ++ " ."),
    Config.

end_per_suite(Config) ->
    Config.

init_per_group(Group = escript, Config) ->
    case os:getenv("ERLUP") of
        Path when is_list(Path) ->
            [{group, Group}, {command, Path ++ " "} | Config]
    end;
init_per_group(Group = rebar3_plugin, Config) ->
    case os:getenv("REBAR") of
        Path when is_list(Path) ->
            [{group, Group}, {command, Path ++ " erlup "}, {help, Path ++ " help erlup "} | Config]
    end.

end_per_group(_, Config) ->
    Config.

init_per_testcase(_, Config) ->
    case os:getenv("REBAR") of
        Path when is_list(Path) ->
            Spam1Dir = filename:join([?config(priv_dir, Config), "spam_v1"]),
            Spam2Dir = filename:join([?config(priv_dir, Config), "spam_v2"]),
            sh(Spam1Dir, Path ++ " compile"),
            sh(Spam2Dir, Path ++ " compile"),
            Config
    end.

end_per_testcase(_, Config) ->
    case os:getenv("REBAR") of
        Path when is_list(Path) ->
            Spam1Dir = filename:join([?config(priv_dir, Config), "spam_v1"]),
            Spam2Dir = filename:join([?config(priv_dir, Config), "spam_v2"]),
            sh(Spam1Dir, Path ++ " clean -a"),
            sh(Spam2Dir, Path ++ " clean -a"),
            Config
    end.

%%----------------------------------------------------------------------------------------------------------------------
%% Test Cases
%%----------------------------------------------------------------------------------------------------------------------

help(Config) ->
    Spam1Dir = filename:join([?config(priv_dir, Config), "spam_v1"]),

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
    Spam1Dir = filename:join([?config(priv_dir, Config), "spam_v1"]),
    Command  = ?config(command, Config) ++ "-v 2>/dev/null",
    ct:log("$ ~s~n~s", [Command, Ret = sh(Spam1Dir, Command)]),
    ?assertMatch({match, _}, re:run(Ret, "erlup v[0-9\.]*.*")).

%%----------------------------------------------------------------------------------------------------------------------
%% Internal Functions
%%----------------------------------------------------------------------------------------------------------------------

sh(Dir, Command) ->
    {ok, Cwd} = file:get_cwd(),
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
