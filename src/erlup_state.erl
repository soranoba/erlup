%% @copyright 2016 Hinagiku Soranoba All Rights Reserved.
%%
%% @doc erlup configure.
-module(erlup_state).

%%----------------------------------------------------------------------------------------------------------------------
%% Exported API
%%----------------------------------------------------------------------------------------------------------------------
-export([
         new/1,
         applies/2,
         mod_deps/1,
         extra/2,
         set_sedargs/3,
         get/2,
         get/3,
         put/3
        ]).

-export_type([
              t/0
             ]).

%%----------------------------------------------------------------------------------------------------------------------
%% Macros and Types
%%----------------------------------------------------------------------------------------------------------------------

-record(?MODULE,
        {
          applies           :: [{Function :: atom(), UpArgs :: [term()], DownArgs :: [term()]}],
          deps              :: [{module(), ModDeps :: [module()]}],
          extra             :: {UpExtra :: term(), DownExtra :: term()},
          sed_args = []     :: [{Before :: term(), After :: term()}],
          dict = dict:new() :: term() % dict
        }).
-opaque t() :: #?MODULE{}.

%%----------------------------------------------------------------------------------------------------------------------
%% Exported Functions
%%----------------------------------------------------------------------------------------------------------------------

-spec new([{atom(), term()}]) -> t().
new(List) ->
    Appup   = proplists:get_value(appup, List, []),
    Applies  = proplists:get_value(applies, Appup, []),
    ModDeps = proplists:get_value(deps,   Appup, []),
    Extra   = proplists:get_value(extra,  Appup, {[], []}),
    #?MODULE{applies = Applies, deps = ModDeps, extra = Extra}.

-spec applies(up | down, t()) -> [{Function :: atom(), Args :: [term()]}].
applies(up, #?MODULE{applies = Applies, sed_args = SedArgs}) ->
    [{Function, sed(Args, SedArgs)} || {Function, Args, _} <- Applies];
applies(down, #?MODULE{applies = Applies, sed_args = SedArgs}) ->
    [{Function, sed(Args, SedArgs)} || {Function, _, Args} <- Applies].

-spec mod_deps(t()) -> [{module(), Deps :: [module()]}].
mod_deps(#?MODULE{deps = Deps}) ->
    Deps.

-spec extra(up | down, t()) -> term().
extra(up, #?MODULE{extra = {Extra, _}, sed_args = SedArgs}) ->
    sed(Extra, SedArgs);
extra(down, #?MODULE{extra = {_, Extra}, sed_args = SedArgs}) ->
    sed(Extra, SedArgs).

-spec set_sedargs(term(), term(), t()) -> t().
set_sedargs(Before, After, #?MODULE{sed_args = SedArgs} = State) ->
    State#?MODULE{sed_args = [{Before, After} | proplists:delete(Before, SedArgs)]}.

-spec get(Key :: term(), t()) -> term().
get(Key, #?MODULE{dict = Dict}) ->
    dict:fetch(Key, Dict).

-spec get(Key :: term(), t(), Default :: term()) -> term().
get(Key, #?MODULE{dict = Dict}, Default) ->
    case dict:find(Key, Dict) of
        {ok, V} -> V;
        error   -> Default
    end.

-spec put(Key :: term(), Value :: term(), t()) -> t().
put(Key, Value, #?MODULE{dict = Dict} = State) ->
    State#?MODULE{dict = dict:store(Key, Value, Dict)}.

%%----------------------------------------------------------------------------------------------------------------------
%% Internal Functions
%%----------------------------------------------------------------------------------------------------------------------

%% @doc It replace terms recursively.
-spec sed(term(), [{term(), term()}]) -> term().
sed(Term, SedList) ->
    case proplists:lookup(Term, SedList) of
        none   -> sed_1(Term, SedList);
        {_, X} -> X
    end.

%% @see sed/2
-spec sed_1(term(), [{term(), term()}]) -> term().
sed_1(List, SedList) when is_list(List) ->
    lists:map(fun(X) -> sed(X, SedList) end, List);
sed_1(Tuple, SedList) when is_tuple(Tuple) ->
    list_to_tuple(sed(tuple_to_list(Tuple), SedList));
sed_1(Other, _SedList) ->
    Other.
