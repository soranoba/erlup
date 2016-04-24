%% @copyright 2016 Hinagiku Soranoba All Rights Reserved.
%%
%% @doc Add a relup to tar for the upgrade / downgrade.
%%
-module(erlup_tarup).

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
%% Macros and Types
%%----------------------------------------------------------------------------------------------------------------------

%%----------------------------------------------------------------------------------------------------------------------
%% Exported Functions
%%----------------------------------------------------------------------------------------------------------------------

%% @doc Add a relup to tar for the upgrade / downgrade.
-spec do([file:filename()], [string()], file:filename(), erlup_state:t()) -> ok.
do(Dirs, PreviousVsns, Tar, State) ->
    TempDir = ec_file:insecure_mkdtemp(),
    ?DEBUG("working directory : ~s", [TempDir]),
    try
        update_tar(Dirs, TempDir, PreviousVsns, Tar, State)
    after
        ec_file:remove(TempDir, [recursive]),
        ?DEBUG("clean tempdir : ~s", [TempDir])
    end.

%%----------------------------------------------------------------------------------------------------------------------
%% 'provider' Callback Functions
%%----------------------------------------------------------------------------------------------------------------------

%% @private
-spec init(rebar_state:t()) -> {ok, rebar_state:t()}.
init(State) ->
    Provider = providers:create([
                                 {name, tarup},
                                 {namespace, erlup},
                                 {module, ?MODULE},
                                 {bare, true},
                                 {deps, []},
                                 {opts, erlup_rebar3:opts("tarup")},
                                 {short_desc, "Add a relup to tar"}
                                ]),
    {ok, rebar_state:add_provider(State, Provider)}.

%% @private
-spec do(rebar_state:t()) -> {ok, rebar_state:t()} | {error, string()}.
do(State) ->
    erlup_rebar3:do("tarup", State).

%% @private
-spec format_error(iodata()) -> iolist().
format_error(Reason) ->
    io_lib:format("~s", [Reason]).

%%----------------------------------------------------------------------------------------------------------------------
%% Internal Functions
%%----------------------------------------------------------------------------------------------------------------------

-spec update_tar([file:filename()], file:filename(), [string()], file:filename(), erlup_state:t()) -> ok.
update_tar(Dirs, TempDir, PreviousVsns0, Tar, State) ->
    case erl_tar:extract(Tar, [{cwd, TempDir}, compressed]) of
        ok                    -> ok;
        {error, {_, Reason0}} -> ?throw(erl_tar:format_error(Reason0))
    end,
    CurrentVsn = case erlup_utils:lookup_current_vsn(TempDir) of
                     {ok, CurrentVsn0} -> CurrentVsn0;
                     {error, Reason1}  -> ?throw(Reason1)
                 end,
    PreviousVsns = lists:delete(CurrentVsn, PreviousVsns0),
    ok = erlup_appup:do([TempDir | Dirs], PreviousVsns, CurrentVsn, State),
    ok = erlup_relup:do([TempDir | Dirs], PreviousVsns, CurrentVsn, State),
    ok = clean_appup(TempDir),
    case erl_tar:create(Tar, [{X, filename:join(TempDir, X)} || X <- filelib:wildcard("*", TempDir)]) of
        ok                    -> ok;
        {error, {_, Reason2}} -> ?throw(erl_tar:format_error(Reason2))
    end,
    ?INFO("update ~s", [Tar]).

-spec clean_appup(file:filename_all()) -> ok.
clean_appup(Dir) ->
    lists:foreach(fun(X) -> file:delete(X) end, filelib:wildcard(filename:join([Dir, "**", "*.appup"]))).
