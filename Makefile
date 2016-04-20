all: escriptize

escriptize:
	@./rebar3 as prod escriptize
	@cp _build/prod/bin/erlup .
