%% @copyright 2016 Hinagiku Soranoba All Rights Reserved.
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
-spec init(rebar_state:t()) -> {ok, rebar_state:t()}.
init(State0) ->
    Mods = [
            erlup_appup
           ],
    lists:foldl(fun(Mod, {ok, State}) -> Mod:init(State) end, {ok, State0}, Mods).

%% escript.
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
    ConfPath  = proplists:get_value(conf, Options, "erlup.config"),
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

do_task("appup", Options, State) ->
    Previous = proplists:get_value(previous, Options, ""),
    Current  = proplists:get_value(current,  Options, ""),
    Dirs     = binary:split(proplists:get_value(dirs, Options, <<>>), <<",">>, [trim, global]),
    erlup_appup:do(Dirs, Previous, Current, State),
    ok.

-spec escript_opt_specs() -> [getopt:option_spec()].
escript_opt_specs() ->
    [
     { task, undefined,  undefined,    string, "Task to run"},
     {    help,        $h,  undefined, undefined, "Display this help"},
     {    conf, undefined,     "conf",    string, "Path of configuration file [default: erlup.conf]"},
     { current,        $c,  undefined,    string, "Vsn of the current release"},
     {previous,        $p,  undefined,    string, "Vsn of the previous release"},
     {    dirs, undefined,      "dir",    binary, "List of release directory (e.g. rel/${APP})"}
    ].

-spec escript_opt_specs(string()) -> [getopt:option_spec()].
escript_opt_specs(Task) ->
    lists:filter(fun(X) -> lists:member(element(1, X), task_opts(Task)) end,
                 escript_opt_specs()).

-spec task_opts(Task :: string()) -> [OptKey :: atom()].
task_opts("appup") -> [current, previous, conf, dirs, help];
task_opts("")      -> [help, task, conf];
task_opts(_)       -> []. % not supported
