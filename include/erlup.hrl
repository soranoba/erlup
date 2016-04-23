%% @copyright 2016 Hinagiku Soranoba All Rights Reserved.

-define(IIF(Cond, TValue, FValue), case Cond of true -> TValue; false -> FValue end).
-define(IF(Cond, TValue),          ((Cond) andalso TValue)).

-define(INFO(Format, Args),  rebar_api:info("[erlup_appup] " ++ Format, Args)).
-define(DEBUG(Format, Args), rebar_api:debug("[erlup_appup] " ++ Format, Args)).
-define(WARN(Format, Args),  rebar_api:warn("[erlup_appup] " ++ Format, Args)).
-define(ERROR(Format, Args), rebar_api:error("[erlup_appup] " ++ Format, Args)).

-define(throw(Msg), throw({error, {?MODULE, Msg}})).
