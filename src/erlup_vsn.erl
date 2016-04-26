%% @copyright 2016 Hinagiku Soranoba All Rights Reserved.
%%
%% @doc Display the release vsn.
%%
-module(erlup_vsn).

-include("erlup.hrl").

%%----------------------------------------------------------------------------------------------------------------------
%% Exported API
%%----------------------------------------------------------------------------------------------------------------------
-export([
         do/2
        ]).

%%----------------------------------------------------------------------------------------------------------------------
%% 'provider' Callback API
%%----------------------------------------------------------------------------------------------------------------------
-export([init/1, do/1, format_error/1]).

%%----------------------------------------------------------------------------------------------------------------------
%% Exported Functions
%%----------------------------------------------------------------------------------------------------------------------

%% @doc Display the release vsn.
-spec do(file:filename(), erlup_state:t()) -> ok.
do(Dir, _State) ->
    case erlup_utils:lookup_current_vsn(Dir) of
        {ok, CurrentVsn} ->
            io:format("~s~n", [CurrentVsn]);
        {error, Reason} ->
            ?throw(Reason)
    end.

%%----------------------------------------------------------------------------------------------------------------------
%% 'provider' Callback Functions
%%----------------------------------------------------------------------------------------------------------------------

%% @private
-spec init(rebar_state:t()) -> {ok, rebar_state:t()}.
init(State) ->
    Provider = providers:create([
                                 {name, vsn},
                                 {namespace, erlup},
                                 {module, ?MODULE},
                                 {bare, true},
                                 {deps, []},
                                 {opts, erlup_rebar3:opts("vsn")},
                                 {short_desc, "Display the release vsn"}
                                ]),
    {ok, rebar_state:add_provider(State, Provider)}.

%% @private
-spec do(rebar_state:t()) -> {ok, rebar_state:t()} | {error, string()}.
do(State) ->
    erlup_rebar3:do("vsn", State).

%% @private
-spec format_error(iodata()) -> iolist().
format_error(Reason) ->
    io_lib:format("~s", [Reason]).
