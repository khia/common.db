APP=$(shell ls src/*.app.src | sed -e 's/src\///g' | sed -e 's/.app.src//g')
NODE=$(subst .,_,$(APP))

all: compile

rebar:
# The officiial version has problem with running hooks
#	wget http://hg.basho.com/rebar/downloads/rebar
	mkdir -p REBAR
	cd REBAR ; git clone git@github.com:khia/rebar.git ; cd rebar ; ./bootstrap
	cp REBAR/rebar/rebar .
	chmod u+x rebar
	rm -rf REBAR

deps: rebar
	./rebar get-deps

compile: deps
	./rebar compile

release: compile
	rm -rf rel
	mkdir rel ; cd rel ; ../rebar create-node nodeid=$(NODE)
	cd ..
	cp reltool.config rel/
	mkdir -p rel/apps/$(APP)
	cp -R ebin rel/apps/$(APP)
	./rebar generate
	rm -rf rel/apps

console: release
	sh rel/$(NODE)/bin/$(NODE) console

clean:
	./rebar clean