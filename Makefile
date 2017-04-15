.PHONY: ps erl all test

all: ps erl

ps:
	psc-package sources | xargs pserlc --no-opts 'src/**/*.purs' 'test/**/*.purs'

test: erl
	erl -pa ebin -noshell -eval '(test_main@ps:main@c())()' -eval 'init:stop()'

erl: ps
	mkdir -p ebin
	erlc -o ebin/ +debug_info output/*/*.erl
