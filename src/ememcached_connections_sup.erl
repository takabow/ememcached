-module(ememcached_connections_sup).

%% API
-export([start_link/0,start_worker/2]).

%% supervisor
-export([init/1]).

%% ===================================================================
%% API
%% ===================================================================
start_link() ->
	proc_lib:start_link(?MODULE, init, [self()]).

start_worker(Pid, Socket) ->
	%% Pid = ememcached_connection_sup の Pid
	Pid ! {?MODULE, start_worker, self(), Socket},
	receive
		Pid -> ok
	end.

%% ===================================================================
%% Supervisor callback
%% ===================================================================
init(Parent) ->
	process_flag(trap_exit,true),
	register(ememcached_connections_sup,self()),
	ok = proc_lib:init_ack(Parent, {ok, self()}),
	loop().

%% ===================================================================
%% Internal
%% ===================================================================
loop() ->
	receive
		{?MODULE, start_worker, To, Socket} ->
			case ememcached_worker:start_link(Socket) of
				{ok, Pid} ->
					gen_tcp:controlling_process(Socket, Pid),
					Pid ! {ping},
					put(Pid,true),
					To ! self(),
					loop();
				_ ->
					To ! self(),
					loop()
			end
	end.

