%% @copyright 2016 Hinagiku Soranoba All Rights Reserved.
%% @private

-module(erlup_utils).

-include("erlup.hrl").

%%----------------------------------------------------------------------------------------------------------------------
%% Exported API
%%----------------------------------------------------------------------------------------------------------------------
-export([
         get_current_vsn/1,
         find_rel/2,
         base_dir/1,
         vsn_libs/1,
         default_current_vsn/1,
         default_previous_vsn/2
        ]).

%%----------------------------------------------------------------------------------------------------------------------
%% Exported Functions
%%----------------------------------------------------------------------------------------------------------------------

-spec get_current_vsn(string()) -> {ok, Vsn :: string()} | {error, string()}.
get_current_vsn(Dir) ->
    case file:read_file(Path = filename:join([Dir, "releases", "start_erl.data"])) of
        {ok, Bin} ->
            case binary:split(Bin, [<<"\n">>, <<" ">>], [global, trim]) of
                [_, Vsn] -> {ok, binary_to_list(Vsn)};
                _        -> {error, "Invalid format. " ++ Path}
            end;
        {error, Reason} ->
            {error, file:format_error(Reason) ++ " " ++ Path}
    end.

base_dir(Path) ->
    case filename:extension(Path) of
        ".rel"   -> filename:dirname(filename:dirname(filename:dirname(Path)));
        ".appup" -> filename:dirname(filename:dirname(filename:dirname(filename:dirname(Path))));
        ".app"   -> filename:dirname(filename:dirname(filename:dirname(filename:dirname(Path))))
    end.

find_rel([], Vsn) ->
    {error, Vsn ++ ".rel not found"};
find_rel([Dir | Dirs], Vsn) ->
    case filelib:wildcard(filename:join([Dir, "releases", Vsn,  "*.rel"])) of
        []        -> find_rel(Dirs, Vsn);
        [RelFile] -> {ok, RelFile};
        [_|_]     -> {error, "*.rel there is more than one"}
    end.

-spec vsn_libs(file:filename()) -> {ok, [{Application :: atom(), Vsn :: string()}]} | {error, string()}.
vsn_libs(RelFile) ->
    case file:consult(RelFile) of
        {ok, [{release, _, _, AppVsns}]} -> {ok, AppVsns};
        {ok, _} ->
            {error, "Invalid format. " ++ RelFile};
        {error, Reason} ->
            {error, file:format_error(Reason) ++ " " ++ RelFile}
    end.

-spec default_current_vsn(file:filename()) -> Vsn :: string().
default_current_vsn(Dir) ->
    case erlup_utils:get_current_vsn(Dir) of
        {ok, CurrentVsn} ->
            CurrentVsn;
        {error, Reason} ->
            ?WARN("~s", [Reason]),
            ""
    end.

-spec default_previous_vsn(file:filename(), string()) -> Vsn :: string().
default_previous_vsn(Dir, CurrentVsn) ->
    Vsns = lists:filtermap(fun(X) -> ?IIF(filelib:is_dir(X), {true, filename:basename(X)}, false) end,
                           filelib:wildcard(filename:join([Dir, "releases", "*"]))),
    ?DEBUG("vsns = ~p", [Vsns]),
    case lists:reverse(lists:filter(fun(X) -> X < CurrentVsn end, lists:usort(Vsns))) of
        [PreviousVsn | _] -> PreviousVsn;
        []                -> ""
    end.
