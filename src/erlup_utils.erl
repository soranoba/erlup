%% @copyright 2016 Hinagiku Soranoba All Rights Reserved.
%%
%% @doc Utility functions
%% @private

-module(erlup_utils).

-include("erlup.hrl").

%%----------------------------------------------------------------------------------------------------------------------
%% Exported API
%%----------------------------------------------------------------------------------------------------------------------
-export([
         list_to_integer_if_possible/1,
         to_string/1,
         split/2,
         absname/1,

         base_dir/1,

         find_rels/1,
         lookup_current_vsn/1,
         lookup_include_libs/1,

         sort_vsns/1
        ]).

%%----------------------------------------------------------------------------------------------------------------------
%% Exported Functions
%%----------------------------------------------------------------------------------------------------------------------

%% @doc Convert to integer from string, if possible.
-spec list_to_integer_if_possible(string()) -> integer() | string().
list_to_integer_if_possible(X) ->
    case catch list_to_integer(X) of
        Int when is_integer(Int) -> Int;
        _                        -> X
    end.

%% @doc Convert to string, if necessary.
-spec to_string(binary() | string()) -> string().
to_string(Bin) when is_binary(Bin) ->
    binary_to_list(Bin);
to_string(Str) when is_list(Str) ->
    Str.

%% @doc Split the list by the position of the Term.
%%
%% The Term is erased.
-spec split([term()], term()) -> {[term()], [term()]}.
split(List, Term) ->
    case lists:splitwith(fun(X) -> X =/= Term end, List) of
        {X, []}    -> {X, []};
        {X, [_|Y]} -> {X, Y}
    end.

%% @doc Given a file path, return a absolute file path.
%%
%% It differs to `filename:absname/1', `..' and `.' are not included.
-spec absname(file:filename_all()) -> file:filename_all().
absname(Path) ->
    Ret = lists:foldl(fun(X, Acc) when X =:= <<".">>; X =:= "." ->
                              Acc;
                         (X, Acc) when X =:= <<"..">>; X =:= ".." ->
                              ?IIF(Acc =:= [], [], tl(Acc));
                         (X, Acc) ->
                              [X | Acc]
                      end, [], filename:split(filename:absname(Path))),
    filename:join(lists:reverse(Ret)).

%% @doc Given a file path, return the release base file path.
-spec base_dir(file:filename_all()) -> file:filename().
base_dir(Path) ->
    AbsPath = absname(Path),
    Ext     = filename:extension(AbsPath),
    Base    = filename:basename(AbsPath),
    Mapping = [
               {".rel",     "myapp/releases/vsn/myapp.rel",         "myapp"},
               {"relup",    "myapp/releases/vsn/relup",             "myapp"},
               {".appup",   "myapp/lib/mymod-vsn/ebin/myapp.appup", "myapp"},
               {".app",     "myapp/lib/mymod-vsn/ebin/myapp.app",   "myapp"},
               {".beam",    "myapp/lib/mymod-vsn/ebin/mymod.beam",  "myapp"},
               {".data",    "myapp/releases/start_erl.data",        "myapp"},
               {"releases", "myapp/releases",                       "myapp"},
               {"lib",      "myapp/lib",                            "myapp"},
               {"ebin",     "myapp/lib/mymod-vsn/ebin",             "myapp"},
               {"bin",      "myapp/bin",                            "myapp"}
              ],
    case ?IIF(Ext =:= "", lists:keyfind(Base, 1, Mapping), lists:keyfind(Ext, 1, Mapping)) of
        false when Ext =/= "" -> error({not_support_extension, Ext}, [Path]);
        false                 -> Path;
        {_, F, B} ->
            lists:foldl(fun(_, Acc) -> filename:dirname(Acc) end,
                        AbsPath, lists:seq(1, length(filename:split(F)) - length(filename:split(B))))
    end.

%% @doc Find .rel files in the directories.
-spec find_rels([Dir :: file:filename_all()]) -> [{RelName :: string(), Vsn :: string(), file:filename_all()}].
find_rels(Dirs) ->
    Ret = lists:foldl(fun(Dir, Acc) ->
                              Wildcard = filename:join([base_dir(Dir), "releases", "*", "*.rel"]),
                              lists:filtermap(fun(Path) ->
                                                      case file:consult(Path) of
                                                          {ok, [{release, {RelName, RelVsn}, _, _}]} ->
                                                              {true, {RelName, RelVsn, Path}};
                                                          _ ->
                                                              false
                                                      end
                                              end, filelib:wildcard(Wildcard)) ++ Acc
                      end, [], Dirs),
    lists:ukeysort(3, Ret).

%% @doc Lookup the current version of a release that given directory is belongs.
-spec lookup_current_vsn(file:filename_all()) -> {ok, Vsn :: string()} | {error, string()}.
lookup_current_vsn(Path0) ->
    Path = filename:join([base_dir(Path0), "releases", "start_erl.data"]),
    case file:read_file(Path) of
        {ok, Bin} ->
            case binary:split(Bin, [<<"\n">>, <<" ">>], [global, trim]) of
                [_, Vsn] -> {ok, to_string(Vsn)};
                _        -> {error, "Invalid format. " ++ to_string(Path)}
            end;
        {error, Reason} ->
            {error, file:format_error(Reason) ++ " " ++ to_string(Path)}
    end.

%% @doc Lookup the application name and vsn of the library.
-spec lookup_include_libs(file:filename()) -> {ok, [{Application :: atom(), Vsn :: string()}]} | {error, string()}.
lookup_include_libs(RelFile) ->
    case file:consult(RelFile) of
        {ok, [{release, _, _, AppVsns}]} -> {ok, AppVsns};
        {ok, _} ->
            {error, "Invalid format. " ++ RelFile};
        {error, Reason} ->
            {error, file:format_error(Reason) ++ " " ++ RelFile}
    end.

%% @doc Sort the vsns to the old version order.
%%
%% MUST be the form of <a href="http://semver.org/">Semantic Versioning</a>
-spec sort_vsns([Vsn :: string()]) -> [string()].
sort_vsns(Vsns) ->
    VersionFun
        = fun(Vsn0) ->
                  %% remove a prefix (ver1.2.3 -> 1.2.3)
                  {_Prefix, Vsn1} = lists:splitwith(fun(X) ->
                                                            not ($0 =< X andalso $9 >= X)
                                                    end, Vsn0),
                  %% remove a build metadata (1.2.3+buildg1234 -> 1.2.3)
                  {Vsn2, _Suffix} = split(Vsn1, $+),
                  %% separates the pre-release version and the release version (Sem1-Sem2)
                  {Sem1, Sem2}    = split(Vsn2, $-),

                  SemVer1 = lists:map(fun list_to_integer_if_possible/1, string:tokens(Sem1, ".")),
                  SemVer2 = lists:map(fun list_to_integer_if_possible/1, string:tokens(Sem2, ".")),
                  %% binary is grater than list.
                  {SemVer1, ?IIF(SemVer2 =:= [], <<>>, SemVer2)}
          end,
    lists:sort(fun(X, Y) -> VersionFun(X) < VersionFun(Y) end, Vsns).
