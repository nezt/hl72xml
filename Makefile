DIALYZER=dialyzer
REBAR=./rebar

.PHONY: start

compile: rebar src/*.erl
	@$(REBAR) compile

clean: rebar ebin
	@$(REBAR) clean
	@rm -rf ebin

start: ebin/*.beam
	(erl -pa ebin -eval 'ok = application:start(hl72xml)')

edoc:
	@$(REBAR) doc

dialyzer: ebin/*.beam
	@$(DIALYZER) ebin/ --plt $(HOME)/.dialyzer_plt -Wunmatched_returns \
    			        -Werror_handling -Wrace_conditions -Wbehaviours \
     				-Wunderspecs
