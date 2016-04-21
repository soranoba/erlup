all: compile eunit escriptize doc

.PHONY: doc

escriptize:
	@./rebar3 as escript escriptize
	@cp _build/escript/bin/erlup .

compile:
	@./rebar3 compile

eunit:
	@./rebar3 eunit

doc:
	@./rebar3 as dev edoc
