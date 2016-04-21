%% @copyright 2016 Hinagiku Soranoba All Rights Reserved.
%%
%% @doc A escript to support the upgrade and downgrade of OTP application.
%%
%% ### Log level
%% It use `rebar_log'.
%% So, you can be set in the same manner as in the rebar3.
%%
%% ```
%% $ QUIET=1 erlup    # silent mode
%% $ DEBUG=1 erlup    # debug mode
%% '''
%%
-module(erlup).

-include("erlup.hrl").

%%----------------------------------------------------------------------------------------------------------------------
%% Exported API
%%----------------------------------------------------------------------------------------------------------------------
-export([
         main/1, init/1
        ]).

%%----------------------------------------------------------------------------------------------------------------------
%% Exported Functions
%%----------------------------------------------------------------------------------------------------------------------

%% rebar3 plugins.
%%
%% @private
-spec init(rebar_state:t()) -> {ok, rebar_state:t()}.
init(State0) ->
    Mods = [
            erlup_appup
           ],
    lists:foldl(fun(Mod, {ok, State}) -> Mod:init(State) end, {ok, State0}, Mods).

%% escript.
%%
%% @private
main(Args) ->
    init_log(),
    case getopt:parse(escript_opt_specs(), Args) of
        {ok, {Options, _}} ->
            DoHelp  = proplists:get_value(help, Options, false),
            Task = proplists:get_value(task, Options, ""),
            case escript_opt_specs(Task) of
                [] ->
                    ?ERROR("Task ~s not found", [Task]), halt(1);
                Specs when DoHelp; Task =:= "" ->
                    case Task =:= "" of
                        true ->
                            getopt:usage(Specs, "erlup"),
                            providers:help(rebar_state:providers(element(2, init(rebar_state:new()))));
                        false ->
                            getopt:usage(Specs, "erlup" ++ " " ++ Task)
                    end,
                    halt(0);
                _ ->
                    try
                        do_task(Task, Options)
                    catch
                        throw:{error, {Module, Str}} when is_atom(Module) ->
                            ?ERROR("~s", [Module:format_error(Str)]),
                            halt(1)
                    end
            end;
        {error, Reason} ->
            ?ERROR("~s", [getopt:format_error(escript_opt_specs(), Reason)]),
            halt(1)
    end,
    halt(0).

%%----------------------------------------------------------------------------------------------------------------------
%% Internal Functions
%%----------------------------------------------------------------------------------------------------------------------

-spec init_log() -> ok.
init_log() ->
    LogLevel = case os:getenv("QUIET") of
                   false ->
                       DefaultLevel = rebar_log:default_level(),
                       case os:getenv("DEBUG") of
                           false -> DefaultLevel;
                           _     -> DefaultLevel + 3
                       end;
                   _ ->
                       rebar_log:error_level()
               end,
    rebar_log:init(command_line, LogLevel).

-spec do_task(string(), [{atom(), term()}]) -> ok.
do_task(Task, Options) ->
    ConfPath  = proplists:get_value(conf, Options),
    ErlupConf = case file:consult(ConfPath) of
                    {ok, Terms} ->
                        case proplists:lookup(erlup, Terms) of
                            none            -> Terms;
                            {_, ErlupConf0} -> ErlupConf0
                        end;
                    {error, Reason} ->
                        ?WARN("~s", [file:format_error(Reason) ++ " " ++ ConfPath]),
                        []
                end,
    do_task(Task, Options, erlup_state:new(ErlupConf)).

-spec do_task(string(), [{atom(), term()}], erlup_state:t()) -> ok.
do_task("appup", Options, State) ->
    CurrentVsn   = proplists:get_value(current, Options),
    PreviousVsns = [erlup_utils:to_string(X)
                    || X <- binary:split(proplists:get_value(previous, Options), <<",">>, [trim, global])],
    Dirs         = [erlup_utils:to_string(X)
                    || X <- binary:split(proplists:get_value(dirs, Options), <<",">>, [trim, global])],
    case {CurrentVsn, PreviousVsns} of
        {undefined, []} -> erlup_appup:do(Dirs, State);
        {undefined, _}  -> erlup_appup:do(Dirs, PreviousVsns, State);
        {_,         []} -> erlup_appup:do(Dirs, CurrentVsn, State);
        _               -> erlup_appup:do(Dirs, PreviousVsns, CurrentVsn, State)
    end,
    ok.

-spec escript_opt_specs() -> [getopt:option_spec()].
escript_opt_specs() ->
    [
     {    task, undefined,  undefined,                 string, "Task to run"},
     {    help,        $h,  undefined,              undefined, "Display this help"},
     {    conf, undefined,     "conf", {string, "erlup.conf"}, "Path of configuration file [default: erlup.conf]"},
     { current,        $c,  undefined,                 string, "Vsn of current release"},
     {previous,        $p,  undefined,         {binary, <<>>}, "List of previous vsn"},
     {    dirs, undefined,      "dir",      {binary, <<".">>}, "List of release directory (e.g. rel/${APP})"}
    ].

-spec escript_opt_specs(string()) -> [getopt:option_spec()].
escript_opt_specs(Task) ->
    lists:filter(fun(X) -> lists:member(element(1, X), task_opts(Task)) end,
                 escript_opt_specs()).

-spec task_opts(Task :: string()) -> [OptKey :: atom()].
task_opts("appup") -> [current, previous, conf, dirs, help];
task_opts("")      -> [help, task, conf];
task_opts(_)       -> []. % not supported
