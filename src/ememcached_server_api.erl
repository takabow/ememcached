%% @doc memcached server API implementation.
-module(ememcached_server_api).

%% API
-export([get/1, set/5, incr/2, decr/2, delete/1]).


%% ===================================================================
%% API
%% ===================================================================
get(Key) ->
    case ememcached_storage:get_internal(Key) of
        {ok, {_, Flags, _, Bytes, Value}} when is_integer(Value) ->
            {ok, list_to_binary(["VALUE ", Key, " ", Flags, " ", integer_to_list(Bytes), "\r\n", integer_to_list(Value), "\r\nEND\r\n"])};
        {ok, {_, Flags, _, Bytes, Value}} ->
            {ok, list_to_binary(["VALUE ", Key, " ", Flags, " ", integer_to_list(Bytes), "\r\n", Value, "\r\nEND\r\n"])};
        notfound ->
            {error, "END\r\n"}

    end.

set(Key, Flags, Exptime, Bytes, Value, inspected_value) ->
    case ememcached_storage:set(Key, Flags, Exptime, Bytes, Value) of
        ok ->
            {ok, "STORED\r\n"};
        _ ->
            {error, "NOT_STORED\r\n"}
    end.

%% A value which consists of decimal digit characters, is stored as integer for incr/decr operation. (e.g. 1234)
%% But the value starts with "0" should be treated as String. (e.g. <<"0123">>)
%% "48 < Digit, Digit < 58" means decimal digit characters except "0" in ASCII code.
set(Key, Flags, Exptime, Bytes, <<Digit:8, _/binary>> = Value) when 48 < Digit, Digit < 58 ->
    try binary_to_integer(Value) of
    %% Value is integer!
        ValueInt ->
            set(Key, Flags, Exptime, Bytes, ValueInt, inspected_value)
    catch
    %% Value starts with number but contains chars. (e.g. "12ab")
        error:_ ->
            set(Key, Flags, Exptime, Bytes, Value, inspected_value)
    end;
%% When given only "0", the value is integer
set(Key, Flags, Exptime, Bytes, <<"0">> = Value) ->
    Zero = binary_to_integer(Value),
    set(Key, Flags, Exptime, Bytes, Zero, inspected_value);
set(Key, Flags, Exptime, Bytes, Value) ->
    set(Key, Flags, Exptime, Bytes, Value, inspected_value).

incr(Key, Value) when is_integer(Value), Value >= 0 ->
    case ememcached_storage:incr(Key, Value) of
        {ok, Result} ->
            {ok, io_lib:format("~p\r\n", [Result])};
        notfound ->
            {notfound, "NOT_FOUND\r\n"};
        X -> io:format("~p", [X])
    end;
incr(_, Value) when is_integer(Value), Value < 0 ->
    {error, "ERROR\r\n"};
incr(Key, Value) ->
    try list_to_integer(binary_to_list(Value)) of
        ValueInt ->
            incr(Key, ValueInt)
    catch
        error:_ ->
            {error, "ERROR\r\n"}
    end.

decr(Key, Value) when is_integer(Value), Value < 0 ->
    case ememcached_storage:decr(Key, Value) of
        {ok, Result} ->
            {ok, io_lib:format("~p\r\n", [Result])};
        notfound ->
            {notfound, "NOT_FOUND\r\n"}
    end;
decr(_, Value) when is_integer(Value), Value >= 0 ->
    {error, "ERROR\r\n"};
decr(Key, Value) ->
    try list_to_integer(binary_to_list(Value)) of
        ValueInt ->
            decr(Key, -ValueInt)
    catch
        error:_ ->
            {error, "ERROR\r\n"}
    end.

delete(Key) ->
    case ememcached_storage:delete(Key) of
        ok ->
            {ok, "DELETED\r\n"};
        notfound ->
            {notfound, "NOT_FOUND\r\n"}
    end.
