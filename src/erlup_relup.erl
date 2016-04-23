%% @copyright 2016 Hinagiku Soranoba All Rights Reserved.
%%
%% @doc Automatically generate the .relup file.
%%
-module(erlup_relup).

-include("erlup.hrl").

%%----------------------------------------------------------------------------------------------------------------------
%% Exported API
%%----------------------------------------------------------------------------------------------------------------------
-export([
         do/4
        ]).

%%----------------------------------------------------------------------------------------------------------------------
%% 'provider' Callback API
%%----------------------------------------------------------------------------------------------------------------------
-export([init/1, do/1, format_error/1]).

%%----------------------------------------------------------------------------------------------------------------------
%% Exported Functions
%%----------------------------------------------------------------------------------------------------------------------

%% @doc Automatically generate the .relup file.
-spec do([file:filename()], string() | [string()], string(), erlup_state:t()) -> ok.
do(Dirs, [X | _] = PreviousVsn, CurrentVsn, State) when not is_list(X) ->
    do(Dirs, [PreviousVsn], CurrentVsn, State);
do(Dirs, PreviousVsns, CurrentVsn, _State) ->
    Rels = erlup_utils:find_rels(Dirs),

    [ToName | FromNames]
        = lists:foldl(fun(PreviousVsn, Acc) ->
                              case lists:keyfind(PreviousVsn, 2, Rels) of
                                  false           -> ?throw("Can not find a rel file. (" ++ PreviousVsn ++ ")");
                                  {_, _, RelFile} -> [filename:rootname(RelFile) | Acc]
                              end
                      end, [], PreviousVsns ++ [CurrentVsn]),
    Ebins = lists:foldl(fun({_, _, RelFile}, Acc) ->
                                case erlup_utils:lookup_include_libs(RelFile) of
                                    {ok, AppVsns} ->
                                        [filename:join([erlup_utils:base_dir(RelFile), "lib",
                                                        atom_to_list(App) ++ "-" ++ AppVsn, "ebin"])
                                         || {App, AppVsn} <- AppVsns] ++ Acc;
                                    {error, Reason} ->
                                        ?throw(Reason)
                                end
                        end, [], Rels),
    Ret = systools:make_relup(ToName, FromNames, FromNames,
                              [{path, Ebins}, {outdir, filename:dirname(ToName)}, silent]),
    case Ret of
        {ok, _, Module, Warnings} ->
            Warn = lists:flatten(Module:format_warning(Warnings)),
            ?IF(Warn =:= [], ?WARN("~s", [Warn])),
            ?INFO("write the relup (~s).", [filename:join(filename:dirname(ToName), "relup")]);
        {error, Module, Error} ->
            ?throw(Module:format_error(Error))
    end.

%%----------------------------------------------------------------------------------------------------------------------
%% 'provider' Callback Functions
%%----------------------------------------------------------------------------------------------------------------------

%% @private
-spec init(rebar_state:t()) -> {ok, rebar_state:t()}.
init(State) ->
    Provider = providers:create([
                                 {name, relup},
                                 {namespace, erlup},
                                 {module, ?MODULE},
                                 {bare, true},
                                 {deps, []},
                                 {opts, erlup_rebar3:opts("relup")},
                                 {short_desc, "Generate the .relup file"}
                                ]),
    {ok, rebar_state:add_provider(State, Provider)}.

%% @private
-spec do(rebar_state:t()) -> {ok, rebar_state:t()} | {error, string()}.
do(State) ->
    erlup_rebar3:do("relup", State).

%% @private
-spec format_error(iodata()) -> iolist().
format_error(Reason) ->
    io_lib:format("~s", [Reason]).
