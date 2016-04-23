%% @copyright 2016 Hinagiku Soranoba All Rights Reserved.
%%
%% @doc Utility for using the erlup from rebar3
%%
-module(erlup_rebar3).

-include("erlup.hrl").

%%----------------------------------------------------------------------------------------------------------------------
%% Exported API
%%----------------------------------------------------------------------------------------------------------------------
-export([
         opts/1, do/2
        ]).

%%----------------------------------------------------------------------------------------------------------------------
%% Exported Functions
%%----------------------------------------------------------------------------------------------------------------------

%% @doc Return the option of command
-spec opts(Task :: string()) -> [getopt:option_spec()].
opts(Task) ->
    lists:filter(fun(X) -> lists:member(element(1, X), [conf, dir, help]) end,
                 erlup:escript_opt_specs(Task)).

%% @doc Execute the task.
-spec do(Task :: string(), rebar_state:t()) -> {ok, rebar_state:t()} | {error, string()}.
do(Task, State) ->
    {Opts, _} = rebar_state:command_parsed_args(State),

    case rebar_state:get(State, relx, undefined) of
        undefined ->
            {error, "Relx configuration is not found."};
        RelxConf  ->
            case lists:keyfind(release, 1, RelxConf) of
                false ->
                    {error, "Configuration of relx is not enough. Please add the release attributes."};
                {_, {Name0, _}, _} when is_atom(Name0) ->
                    Name       = atom_to_list(Name0),
                    Dir        = filename:join([rebar_dir:base_dir(State), "rel", Name]),
                    ErlupState = erlup_state:new(rebar_state:get(State, erlup, [])),

                    ok = erlup:do_task(Task, [{dir, erlup_utils:to_binary(Dir)} | Opts], ErlupState),
                    {ok, State};
                _ ->
                    {error, "Relx configuration is invalid format."}
            end
    end.
