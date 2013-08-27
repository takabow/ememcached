-module(ememcached_sup).

-behaviour(supervisor).

%% API
-export([start_link/2]).

%% Supervisor callbacks
-export([init/1]).

%% Helper macro for declaring children of supervisor
-define(CHILD(I, Type), {I, {I, start_link, []}, permanent, 5000, Type, [I]}).
-define(CHILD(I, Type, Args), {I, {I, start_link, Args}, permanent, 5000, Type, [I]}).

%% ===================================================================
%% API
%% ===================================================================
start_link(Port, AcceptorsNum) ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, [Port, AcceptorsNum]).

%% ===================================================================
%% Supervisor callbacks
%% ===================================================================
init([Port, AcceptorsNum]) ->
    Listener = ?CHILD(ememcached_listener_sup, supervisor, [Port, AcceptorsNum]),

    ememcached_storage = ets:new(ememcached_storage, [set, public, named_table]),
    {ok, {{one_for_one, 5, 10}, [Listener]}}.
