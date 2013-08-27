%% @doc Backend storage using ets.
-module(ememcached_storage).

%% API
-export([get/1, set/5, delete/1, incr/2, decr/2]).
-export([get_internal/1, set_internal/5, delete_internal/1, incr_internal/2, decr_internal/2]).

-define(TAB, ememcached_storage).

%% ===================================================================
%% API
%% ===================================================================
get(Key) ->
    get_internal(Key).

set(Key, Flags, Exptime, Bytes, Value) ->
    set_internal(Key, Flags, Exptime, Bytes, Value).

delete(Key) ->
    case contains_internal(Key) of
        true ->
            delete_internal(Key);
        false ->
            notfound
    end.

incr(Key, Value) ->
    case contains_internal(Key) of
        true ->
            incr_internal(Key, Value);
        false ->
            notfound
    end.

decr(Key, Value) ->
    case contains_internal(Key) of
        true ->
            decr_internal(Key, Value);
        false ->
            notfound
    end.

get_internal(Key) ->
    case ets:lookup(?TAB, Key) of
        [] ->
            notfound;
    %% Entry = Key, Flags, Exptime, Bytes, Value
        [Entry] ->
            {ok, Entry}
    end.

set_internal(Key, Flags, Exptime, _, Value) ->
    case ets:insert(?TAB, {Key, Flags, Exptime, Value}) of
        true -> ok;
        _ -> error
    end.

delete_internal(Key) ->
    case ets:delete(?TAB, Key) of
        true -> ok;
        _ -> error
    end.

incr_internal(Key, Value) when is_integer(Value), Value >= 0 ->
    try ets:update_counter(?TAB, Key, {4, Value}) of
        Result ->
            {ok, Result}
    catch
        error:X ->
            {error, X}
    end.

decr_internal(Key, Value) when is_integer(Value), Value < 0 ->
    try ets:update_counter(?TAB, Key, {4, Value, 0, 0}) of
        Result ->
            {ok, Result}
    catch
        error:X ->
            {error, X}
    end.

contains_internal(Key) ->
    ets:member(?TAB, Key).