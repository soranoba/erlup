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
%% Macros
%%----------------------------------------------------------------------------------------------------------------------

-define(print_error(Format, Args), io:format(standard_error, Format ++ "~n", Args)).

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
    case getopt:parse(escript_opt_specs(), Args) of
        {ok, {Options, _}} ->
            DoHelp  = proplists:get_value(help, Options, false),
            Command = proplists:get_value(command, Options, ""),
            case escript_opt_specs(Command) of
                [] ->
                    ?print_error("Command ~s not found", [Command]), halt(1);
                Specs when DoHelp; Command =:= "" ->
                    getopt:usage(Specs, "erlup" ++ ?IIF(Command == "", "", " " ++ Command)),
                    halt(0);
                _ ->
                    do_command(Command, Options)
            end;
        {error, Reason} ->
            ?print_error("~s", [getopt:format_error(escript_opt_specs(), Reason)]),
            halt(1)
    end,
    halt(0).

%%----------------------------------------------------------------------------------------------------------------------
%% Internal Functions
%%----------------------------------------------------------------------------------------------------------------------

-spec do_command(string(), [{atom(), term()}]) -> ok.
do_command("appup", Options) ->
    _Previous = proplists:get_value(previous, Options, "."),
    _Current  = proplists:get_value(current,  Options, "."),
    ok.

-spec escript_opt_specs() -> [getopt:option_spec()].
escript_opt_specs() ->
    [
     {    help,        $h,  undefined, undefined, "Display this help"},
     { command, undefined,  undefined,    string, "Command to run"},
     { current,        $c,  "current",    string, "Directory of the previous release"},
     {previous,        $p, "previous",    string, "Directory of the previous release"}
    ].

-spec escript_opt_specs(string()) -> [getopt:option_spec()].
escript_opt_specs(Command) ->
    lists:filter(fun(X) -> lists:member(element(1, X), command_opts(Command)) end,
                 escript_opt_specs()).

-spec command_opts(Command :: string()) -> [OptKey :: atom()].
command_opts("appup") -> [current, previous];
command_opts("")      -> [help, command];
command_opts(_)       -> []. % not supported
