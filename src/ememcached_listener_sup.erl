-module(ememcached_listener_sup).

-behaviour(supervisor).

%% API
-export([start_link/2]).

%% Supervisor callbacks
-export([init/1]).

%% Helper macro for declaring children of supervisor
-define(CHILD(I, Type), {I, {I, start_link, []}, permanent, infinity, Type, [I]}).
-define(CHILD(I, Type, Args), {I, {I, start_link, Args}, permanent, infinity, Type, [I]}).

%% ===================================================================
%% API
%% ===================================================================
start_link(Port, AcceptorsNum) ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, [Port, AcceptorsNum]).

%% ===================================================================
%% Supervisor callbacks
%% ===================================================================
init([Port, AcceptorsNum]) ->
    ConnectionsSup = ?CHILD(ememcached_connections_sup, supervisor),
    AcceptorsSup = ?CHILD(ememcached_acceptors_sup, supervisor, [Port, AcceptorsNum]),
    {ok, {{rest_for_one, 10, 10}, [ConnectionsSup, AcceptorsSup]}}.
