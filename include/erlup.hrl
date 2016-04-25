%% @copyright 2016 Hinagiku Soranoba All Rights Reserved.

-define(IIF(Cond, TValue, FValue), case Cond of true -> TValue; false -> FValue end).
-define(IF(Cond, TValue),          ((Cond) andalso TValue)).

-define(INFO(Format, Args),  rebar_api:info("[~p] "  ++ Format, [?MODULE | Args])).
-define(DEBUG(Format, Args), rebar_api:debug("[~p] " ++ Format, [?MODULE | Args])).
-define(WARN(Format, Args),  rebar_api:warn("[~p] "  ++ Format, [?MODULE | Args])).
-define(ERROR(Format, Args), rebar_api:error("[~p] " ++ Format, [?MODULE | Args])).

-define(throw(Msg), throw({error, {?MODULE, Msg}})).
