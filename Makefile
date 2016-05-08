CWD=$(shell pwd)

all: eunit ct xref dialyzer

escriptize:
	@./rebar3 as escript escriptize
	@cp _build/escript/bin/erlup .

compile:
	@./rebar3 compile

eunit:
	@./rebar3 eunit

ct: escriptize
	@REBAR=$(CWD)/rebar3 ERLUP=$(CWD)/erlup ./rebar3 ct

xref:
	@./rebar3 as escript xref

dialyzer:
	@./rebar3 dialyzer
