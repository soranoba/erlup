all: eunit escriptize

escriptize:
	@./rebar3 as escript escriptize
	@cp _build/escript/bin/erlup .

compile:
	@./rebar3 compile

eunit:
	@./rebar3 eunit
