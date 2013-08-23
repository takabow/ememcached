-module(ememcached_storage).

-behaviour(gen_server).

%% API
-export([start_link/0]).

-export([get/1, set/5, delete/1, incr/2, decr/2]).
-export([get_internal/1, set_internal/5, delete_internal/1, incr_internal/2, decr_internal/2]).

%% gen_server
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

-define(TAB, ememcached_storage).
-record(state, {}).

%% ===================================================================
%% API
%% ===================================================================
start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

get(Key) ->
    %% gen_server:call(?MODULE, {get, [Key]}).
    get_internal(Key).

set(Key, Flags, Exptime, Bytes, Value) ->
    gen_server:call(?MODULE, {set, [Key, Flags, Exptime, Bytes, Value]}).

delete(Key) ->
    case contains_internal(Key) of
        true ->
            gen_server:call(?MODULE, {delete, [Key]});
        false ->
            notfound
    end.

incr(Key, Value) ->
    case contains_internal(Key) of
        true ->
            gen_server:call(?MODULE, {incr, [Key, Value]});
        false ->
            notfound
    end.

decr(Key, Value) ->
    case contains_internal(Key) of
        true ->
            gen_server:call(?MODULE, {decr, [Key, Value]});
        false ->
            notfound
    end.

get_internal(Key) ->
    case ets:lookup(?TAB, Key) of
        [] ->
            notfound;
    %% Entry = Flags, Exptime, Bytes, Value
        [Entry] ->
            {ok, Entry}
    end.

set_internal(Key, Flags, Exptime, Bytes, Value) ->
    case ets:insert(?TAB, {Key, Flags, Exptime, Bytes, Value}) of
        true -> ok;
        _ -> error
    end.

delete_internal(Key) ->
    case ets:delete(?TAB, Key) of
        true -> ok;
        _ -> error
    end.

incr_internal(Key, Value) when is_integer(Value), Value >= 0 ->
    try ets:update_counter(?TAB, Key, {5, Value}) of
        Result ->
            {ok, Result}
    catch
        error:X ->
            {error, X}
    end.

decr_internal(Key, Value) when is_integer(Value), Value < 0 ->
    try ets:update_counter(?TAB, Key, {5, Value, 0, 0}) of
        Result ->
            {ok, Result}
    catch
        error:X ->
            {error, X}
    end.

contains_internal(Key) ->
    ets:member(?TAB, Key).

%% ===================================================================
%% Genserver callbacks
%% ===================================================================	
init(_Args) ->
    {ok, #state{}}.

handle_call({get, [Key]}, _From, State) ->
    {reply, get_internal(Key), State};

handle_call({set, [Key, Flags, Exptime, Bytes, Value]}, _From, State) ->
    {reply, set_internal(Key, Flags, Exptime, Bytes, Value), State};

handle_call({delete, [Key]}, _From, State) ->
    {reply, delete_internal(Key), State};

handle_call({incr, [Key, Value]}, _From, State) ->
    {reply, incr_internal(Key, Value), State};

handle_call({decr, [Key, Value]}, _From, State) ->
    {reply, decr_internal(Key, Value), State};

handle_call(_Request, _From, State) ->
    {noreply, State}.

handle_cast(_Request, State) ->
    {noreply, State}.

handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.
