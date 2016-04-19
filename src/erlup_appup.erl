%% @copyright 2016 Hinagiku Soranoba All Rights Reserved.

-module(erlup_appup).
-behaviour(provider).

-include("erlup.hrl").

%%----------------------------------------------------------------------------------------------------------------------
%% Exported API
%%----------------------------------------------------------------------------------------------------------------------

%%----------------------------------------------------------------------------------------------------------------------
%% 'provider' Callback API
%%----------------------------------------------------------------------------------------------------------------------
-export([init/1, do/1, format_error/1]).

%%----------------------------------------------------------------------------------------------------------------------
%% Macros and Types
%%----------------------------------------------------------------------------------------------------------------------

-type instruction() :: tuple().
-record(?MODULE,
        {
        }).
-type state() :: #?MODULE{}.

-define(INFO(Format, Args),  rebar_api:info("[erlup_appup] " ++ Format, Args)).
-define(DEBUG(Format, Args), rebar_api:debug("[erlup_appup] " ++ Format, Args)).
-define(WARN(Format, Args),  rebar_api:warn("[erlup_appup] " ++ Format, Args)).
-define(ERROR(Format, Args), rebar_api:error("[erlup_appup] " ++ Format, Args)).

%%----------------------------------------------------------------------------------------------------------------------
%% 'provider' Callback Functions
%%----------------------------------------------------------------------------------------------------------------------

-spec init(rebar_state:t()) -> {ok, rebar_state:t()}.
init(State) ->
    Provider = providers:create([
                                 {name, appup},
                                 {namespace, erlup},
                                 {module, ?MODULE},
                                 {bare, true},
                                 {deps, []},
                                 {opts, opts()},
                                 {short_desc, "Generate the .appup file"}
                                ]),
    {ok, rebar_state:add_provider(State, Provider)}.

-spec do(rebar_state:t()) -> {ok, rebar_state:t()} | {error, string()}.
do(State) ->
    {Opts, _} = rebar_state:command_parsed_args(State),

    case rebar_state:get(State, relx, undefined) of
        undefined -> {error, "Relx configuration is not found."};
        RelxConf  ->
            case lists:keyfind(release, 1, RelxConf) of
                {_, {Name0, _}, _} when is_atom(Name0) ->
                    Name = atom_to_list(Name0),
                    Dir  = filename:join([rebar_dir:base_dir(State), "rel", Name]),
                    DefaultVsns = default_vsns(Dir),
                    PreviousVsn = proplists:get_value(previous, Opts ++ DefaultVsns, []),
                    CurrentVsn  = proplists:get_value(current, Opts ++ DefaultVsns, []),
                    PreviousVsn =:= [] andalso throw({?MODULE, "Previous vsn is unknown"}),
                    CurrentVsn  =:= [] andalso throw({?MODULE, "Current vsn is unknown. Please run ./rebar3 release"}),
                    do_1(Dir, PreviousVsn, CurrentVsn, []),
                    {ok, State};
                _ ->
                    {error, "Release name is unknown."}
            end
    end.

-spec format_error(iodata()) -> iolist().
format_error(Reason) ->
    io_lib:format("~s", [Reason]).

%%----------------------------------------------------------------------------------------------------------------------
%% Internal Functions
%%----------------------------------------------------------------------------------------------------------------------

-spec opts() -> [getopt:option_spec()].
opts() ->
    [
     {previous, $p, undefined, string, "Previous vsn"},
     {current,  $c, undefined, string, "Current vsn"}
    ].

-spec default_vsns(file:filename()) -> [{current | previous, Vsn :: string()}].
default_vsns(Dir) ->
    Vsns = lists:filtermap(fun(X) -> ?IIF(filelib:is_dir(X), {true, filename:basename(X)}, false) end,
                           filelib:wildcard(filename:join([Dir, "releases", "*"]))),
    ?DEBUG("vsns = ~p", [Vsns]),
    case erlup_utils:get_current_vsn(Dir) of
        {ok, CurrentVsn} ->
            case lists:reverse(lists:filter(fun(X) -> X < CurrentVsn end, lists:usort(Vsns))) of
                [PreviousVsn | _] -> [{current, CurrentVsn}, {previous, PreviousVsn}];
                []                -> [{current, CurrentVsn}]
            end;
        {error, Reason} ->
            ?WARN("~s", [Reason]),
            []
    end.

do_1(Dir, PreviousVsn, CurrentVsn, State) ->
    ?INFO("previous = ~s, current = ~s", [PreviousVsn, CurrentVsn]),
    Fun = fun(Vsn) ->
                  case erlup_utils:vsn_libs(Dir, Vsn) of
                      {ok, AppVsns} ->
                          [begin
                               EbinDir = filename:join([Dir, "lib", atom_to_list(App) ++ "-" ++ AppVsn, "ebin"]),
                               {App, AppVsn, EbinDir}
                           end || {App, AppVsn} <- AppVsns];
                      {error, Reason} ->
                          throw({?MODULE, Reason})
                  end
          end,
    rewrite_appups(Fun(CurrentVsn), Fun(PreviousVsn), State).

rewrite_appups([], _, _) ->
    ok;
rewrite_appups([{App, ToVsn, ToEbinDir} | ToRest], From, State) ->
    case lists:keyfind(App, 1, From) of
        false ->
            rewrite_appups(ToRest, From, State);
        {_, ToVsn, _} ->
            rewrite_appups(ToRest, From, State);
        {_, FromVsn, FromEbinDir} ->
            ?DEBUG("to: ~s, from: ~s", [ToEbinDir, FromEbinDir]),
            AppupPath = filename:join(ToEbinDir, atom_to_list(App) ++ ".appup"),
            {Up, Down} = case file:consult(AppupPath) of
                             {ok, [{ToVsn, Up0, Down0}]} when is_list(Up0), is_list(Down0) ->
                                 {proplists:delete(FromVsn, Up0), proplists:delete(FromVsn, Down0)};
                             {ok, _} ->
                                 ?WARN("the appup (~s) is invalid format. to overwrite.", [AppupPath]),
                                 {[], []};
                             _ ->
                                 {[], []}
                         end,
            Up2   = [{FromVsn, Instructions = application_instructions(ToEbinDir, FromEbinDir, State)} | Up],
            Down2 = [{FromVsn, revert_instructions(Instructions)} | Down],
            case file:write_file(AppupPath, format_appup(ToVsn, Up2, Down2)) of
                ok              -> ok;
                {error, Reason} -> throw({?MODULE, file:format_error(Reason) ++ " " ++ AppupPath})
            end,
            ?INFO("rewrite the appup (~s).", [AppupPath]),
            rewrite_appups(ToRest, From, State)
    end.

-spec revert_instructions([instruction()]) -> [instruction()].
revert_instructions(Insts) ->
    revert_instructions(Insts, []).

-spec revert_instructions([instruction()], [instruction()]) -> [instruction()].
revert_instructions([], Acc) ->
    Acc; % keep the reversing list
revert_instructions([Inst | RestInsts], Acc) when element(1, Inst) =:= add_module ->
    %% {add_module, Mod}, {add_module, Mod, DepMods} -> {delete_module, ...}
    revert_instructions(RestInsts, [setelement(1, Inst, delete_module) | Acc]);
revert_instructions([Inst | RestInsts], Acc) when element(1, Inst) =:= delete_module ->
    %% {delete_module, Mod}, {delete_module, Mod, DepMods} -> {add_module, ...}
    revert_instructions(RestInsts, [setelement(1, Inst, add_module) | Acc]);
revert_instructions([Inst | RestInsts], Acc) ->
    revert_instructions(RestInsts, [Inst | Acc]).

-spec application_instructions(file:filename(), file:filename(), state()) -> [instruction()].
application_instructions(ToEbinDir, FromEbinDir, State) ->
    {Added, Deleted, Differents} = beam_lib:cmp_dirs(ToEbinDir, FromEbinDir),
    CmpFiles = lists:map(fun(X) -> {added, X} end, Added)
        ++ lists:map(fun({X, _}) -> {changed, X} end, Differents)
        ++ lists:map(fun(X) -> {deleted, X} end, Deleted),
    %% TODO: ordering
    application_instructions_1(CmpFiles, State, []).

-spec application_instructions_1([{Cmp, file:filename()}], state(), _) -> [instruction()] when
      Cmp :: added | deleted | changed.
application_instructions_1([], _, Acc) ->
    lists:flatten(Acc);
application_instructions_1([{Cmp, File} | Rest], State, Acc) ->
    {ok, Chunks} = beam_lib:chunks(File, [attributes, exports]),
    application_instructions_1(Rest, State, [module_instructions(Cmp, Chunks, State) | Acc]).

%% @doc Returns the instructions of the module.
-spec module_instructions(Cmp, {module(), beam_lib:chunkdata()}, state()) -> [instruction()] when
      Cmp :: added | deleted | changed.
module_instructions(added, {Module, _}, _) ->
    [{add_module, Module}];
module_instructions(deleted, {Module, _}, _) ->
    [{delete_module, Module}];
module_instructions(changed, {Module, ChunkData}, State) ->
    ModDeps = proplists:get_value(Module, erlup_state:mod_deps(State), []),
    Insts = ?IIF(is_supervisor(ChunkData), [{update, Module, supervisor}], [])
            ++ ?IIF(is_special_process(ChunkData),
                    [{update, Module, {advanced, erlup_state:extra(State)}, ModDeps}], []),
    ?IIF(Insts =:= [], [{load_module, Module, ModDeps}], Insts)
        ++ lists:map(fun(X) -> {apply, X} end, get_applys({Module, ChunkData}, State)).

%%
-spec get_applys({module(), [beam_lib:chunkdata()]}, state()) -> [{module(), Function :: atom(), Args :: term()}].
get_applys({Module, ChunkData}, State) ->
    Exports = proplists:get_value(exports, ChunkData, []),
    lists:filter(fun({Function, Args}) when is_list(Args) ->
                         case lists:member({Function, length(Args)}, Exports) of
                             true  -> {true, {Module, Function, Args}};
                             false -> false
                         end;
                    (_) -> false
                 end, erlup_state:applys(State)).

%% @doc Return the behaviour modules
-spec get_behaviour([beam_lib:chunkdata()]) -> [module()].
get_behaviour(ChunkData) ->
    Attributes = proplists:get_value(attributes, ChunkData),
    proplists:get_value(behavior, Attributes, []) ++ proplists:get_value(behaviour, Attributes, []).

%% @doc Return true, if it is special process. Otherwise false.
-spec is_special_process([beam_lib:chunkdata()]) -> boolean().
is_special_process(ChunkData) ->
    Exports = proplists:get_value(exports, ChunkData, []),
    proplists:is_defined(code_change, Exports) orelse proplists:is_defined(system_code_change, Exports).

%% @doc Return true, if it is supervisor. Otherwise, false.
-spec is_supervisor([beam_lib:chunkdata()]) -> boolean().
is_supervisor(ChunkData) ->
    lists:member(supervisor, get_behaviour(ChunkData)).

%% @doc Returns the data of appup formats in human readable format.
-spec format_appup(string(), [{FromVsn, [instruction()]}], [{FromVsn, [instruction()]}]) -> iodata() when
      FromVsn :: string().
format_appup(ToVsn, Up, Down) ->
    io_lib:format("{~p,~n [~n~s ],~n [~n~s ]}.~n",
                  [ToVsn, format_vsn_instructions(Up), format_vsn_instructions(Down)]).

%% @see format_appup/3
-spec format_vsn_instructions([{FromVsn, [instruction()]}]) -> iodata() when
      FromVsn :: string().
format_vsn_instructions([]) ->
    [];
format_vsn_instructions(VsnInstructions) ->
    string:join(lists:map(fun({FromVsn, Instructions}) ->
                                  io_lib:format("  {~p,~n   [~n~s   ]}",
                                                [FromVsn, format_instructions(Instructions)])
                          end, VsnInstructions), [$,,$\n]) ++ [$\n].

%% @see format_appup/3
-spec format_instructions([instruction()]) -> iodata().
format_instructions([]) ->
    [];
format_instructions(Instructions) ->
    string:join(lists:map(fun(X) -> io_lib:format("    ~p", [X]) end, Instructions), [$,,$\n]) ++ [$\n].
