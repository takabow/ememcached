-module(ememcached_app).

-behaviour(application).

%% Application callbacks
-export([start/2, stop/1]).

%% ===================================================================
%% Application callbacks
%% ===================================================================
start(_StartType, _StartArgs) ->
    {ok, Port} = application:get_env(ememcached, port),
    {ok, AcceptorsNum} = application:get_env(ememcached, acceptors),
    ememcached_sup:start_link(Port, AcceptorsNum).

stop(_State) ->
    ok.
