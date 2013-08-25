%% @doc TCP connection manipulator.
-module(ememcached_worker).

%% API
-export([start_link/1, init/1]).


%% ===================================================================
%% API
%% ===================================================================
start_link(Socket) ->
    Pid = spawn_link(?MODULE, init, [Socket]),
    {ok, Pid}.

init(ConnectionSocket) ->
    ok = accept_ack(),
    loop(ConnectionSocket).

accept_ack() ->
    receive
        ping ->
            ok
    end.

%% ===================================================================
%% Internal
%% ===================================================================
loop(ConnectionSocket) ->
    case gen_tcp:recv(ConnectionSocket, 0, infinity) of
        {ok, Data} ->
            Token = binary:split(Data, [<<" ">>, <<"\r\n">>], [global]),
            Command = lists:filter(fun(X) -> X =/= <<>> end, Token),
            command(ConnectionSocket, Command),
            loop(ConnectionSocket);
        {error, closed} -> ok
    end.


command(ConnectionSocket, [<<"get">>, Key]) ->
    {_, Response} = ememcached_server_api:get(Key),
    gen_tcp:send(ConnectionSocket, Response);
command(ConnectionSocket, [<<"set">>, Key, Flags, Exptime, Bytes]) ->
    try binary_to_integer(Bytes) of
        BytesInt ->
            inet:setopts(ConnectionSocket, [{packet, raw}]),
            case gen_tcp:recv(ConnectionSocket, BytesInt) of
                {ok, Value} ->
                    {_, Response} = ememcached_server_api:set(Key, Flags, Exptime, BytesInt, Value),
                    inet:setopts(ConnectionSocket, [{packet, line}]),
                    gen_tcp:send(ConnectionSocket, Response)
            end
    catch
        error:_ ->
            gen_tcp:send(ConnectionSocket, "ERROR\r\n")
    end;
command(ConnectionSocket, [<<"delete">>, Key]) ->
    {_, Response} = ememcached_server_api:delete(Key),
    gen_tcp:send(ConnectionSocket, Response);
command(ConnectionSocket, [<<"incr">>, Key, Value]) ->
    {_, Response} = ememcached_server_api:incr(Key, Value),
    gen_tcp:send(ConnectionSocket, Response);
command(ConnectionSocket, [<<"decr">>, Key, Value]) ->
    {_, Response} = ememcached_server_api:decr(Key, Value),
    gen_tcp:send(ConnectionSocket, Response);
command(ConnectionSocket, [<<"quit">>, _]) ->
    gen_tcp:close(ConnectionSocket);
command(ConnectionSocket, [<<"quit">>]) ->
    gen_tcp:close(ConnectionSocket);
command(_, []) ->
    ok;
command(ConnectionSocket, _) ->
    gen_tcp:send(ConnectionSocket, "ERROR\r\n").
