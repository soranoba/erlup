%% @copyright 2016 Hinagiku Soranoba All Rights Reserved.

-module(erlup_utils).

%%----------------------------------------------------------------------------------------------------------------------
%% Exported API
%%----------------------------------------------------------------------------------------------------------------------
-export([
         get_current_vsn/1,
         vsn_libs/2
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

-spec vsn_libs(file:filename(), string()) -> {ok, [{Application :: atom(), Vsn :: string()}]} | {error, string()}.
vsn_libs(Dir, Vsn) ->
    case filelib:wildcard(filename:join([Dir, "releases", Vsn, "*.rel"])) of
        [RelFile] ->
            case file:consult(RelFile) of
                {ok, [{release, _, _, AppVsns}]} -> {ok, AppVsns};
                {ok, _} ->
                    {error, "Invalid format. " ++ RelFile};
                {error, Reason} ->
                    {error, file:format_error(Reason) ++ " " ++ RelFile}
            end;
        _ ->
            {error, Vsn ++ " was broken."}
    end.
