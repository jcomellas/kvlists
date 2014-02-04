PROJECT := kvlists

REBAR = $(shell which rebar || echo ./rebar)
ERL := erl
EPATH = -pa ebin -pz deps/*/ebin
TEST_EPATH = -pa .eunit -pz deps/*/ebin
PLT_APPS = $(shell ls $(ERL_LIB_DIR) | grep -v interface | sed -e 's/-[0-9.]*//')
DIALYZER_OPTS=-Wno_return -Wno_undefined_callbacks --fullpath

.PHONY: all build_plt compile configure console deps doc clean depclean depcompile distclean dialyze release telstart test test-console

all: compile

build_plt:
	@dialyzer --build_plt --apps $(PLT_APPS)

compile:
	@rebar skip_deps=true compile

configure:
	@rebar get-deps compile

console:
	$(ERL) -sname $(PROJECT) $(EPATH)

deps:
	@rebar get-deps update-deps

doc:
	@rebar skip_deps=true doc

clean:
	@rebar skip_deps=true clean

depclean:
	@rebar clean

depcompile:
	@rebar compile

distclean:
	@rebar clean delete-deps

dialyze: compile
	@dialyzer $(DIALYZER_OPTS) -r ./ebin/

release: compile
	@rebar generate

start:
	$(ERL) -sname $(PROJECT) $(EPATH) -s $(PROJECT)

test:
	@rebar skip_deps=true ct verbose=1

test-console: test
	$(ERL) -sname $(PROJECT)_test $(TEST_EPATH)

xref:
	@rebar xref
