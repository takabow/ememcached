%% @doc TCP connection acceptor.
-module(ememcached_acceptor).

%% API
-export([start_link/1]).

%% Internal
-export([loop/2]).

%% Helper macro for declaring children of supervisor
-define(CHILD(I, Type), {I, {I, start_link, []}, permanent, 5000, Type, [I]}).
-define(CHILD(I, Type, Args), {I, {I, start_link, Args}, permanent, 5000, Type, [I]}).

%% ===================================================================
%% API
%% ===================================================================
start_link(ListenSocket) ->
    ConnectionsSup = whereis(ememcached_connections_sup),
    Pid = spawn_link(?MODULE, loop, [ListenSocket, ConnectionsSup]),
    {ok, Pid}.

%% ===================================================================
%% Internal
%% ===================================================================
loop(ListenSocket, ConnectionsSup) ->
    _ = case gen_tcp:accept(ListenSocket, infinity) of
            {ok, ConnectionSocket} ->
                gen_tcp:controlling_process(ConnectionSocket, ConnectionsSup),
                ememcached_connections_sup:start_worker(ConnectionsSup, ConnectionSocket);
            {error, Reason} when Reason =/= closed ->
                io:format("~p ememcached_acceptors:loop - connection error~n", [self()]),
                ok
        end,
    loop(ListenSocket, ConnectionsSup).
