all: eunit escriptize xref dialyzer

escriptize:
	@./rebar3 as escript escriptize
	@cp _build/escript/bin/erlup .

compile:
	@./rebar3 compile

eunit:
	@./rebar3 eunit

xref:
	@./rebar3 as escript xref

dialyzer:
	@./rebar3 dialyzer
