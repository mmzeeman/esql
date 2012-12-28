REBAR=./rebar

all: compile

rebar:
	wget https://github.com/downloads/basho/rebar/rebar -O $(REBAR)
	chmod u+x $(REBAR)

compile: rebar
	$(REBAR) compile

test: compile
	$(REBAR) eunit skip_deps=true

clean: rebar
	$(REBAR) clean

distclean: 
	rm $(REBAR)
