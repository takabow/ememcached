
all: rel

compile:
	./rebar compile

clean:
	./rebar clean

generate:
	./rebar generate 

rel: compile generate

