-module(ememcached).

-export([start/0]).


start() ->
	application:start(ememcached).
