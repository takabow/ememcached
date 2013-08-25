This is my practice for learning Erlang.

# Overview

Ememcached is a memcached server implementation written in Erlang.
Currently, Ememcached is partially compatible with memcached.

available commands:

- `get`
- `set` (expire is not supported)
- `delete`
- `incr`
- `decr`

Of course, text portocol only!

# Quick Start

## Requirements

Ememcached requires Erlang R16B01.


## Building Ememcached

Just hit `make` command (or `./rebar compile` and `./rebar generate`)
```
$ cd ememcached
$ make
./rebar compile
==> rel (compile)
==> ememcached (compile)
Compiled src/ememcached_sup.erl
Compiled src/ememcached_worker.erl
Compiled src/ememcached_storage.erl
Compiled src/ememcached_listener_sup.erl
Compiled src/ememcached_connections_sup.erl
Compiled src/ememcached_app.erl
Compiled src/ememcached_acceptors_sup.erl
Compiled src/ememcached_acceptor.erl
Compiled src/ememcached.erl
Compiled src/ememcached_server_api.erl
./rebar generate
==> rel (generate)
``` 

## Starting ememcached
After build, change directory to `rel/ememcached/bin`, then execute following command.

- `./ememcached start` Start the server (default tcp port 11211).
- `./ememcached stop` Stop the server.

To check if the emecached server is running, you can ping to the server.

- `./ememcached ping` Send a ping to the server.

Example:
```
$ cd rel/ememcached/bin
$ ./ememcached start 
$ ./ememcahced ping
pong
$ ./ememcached stop
ok
```

## Options
If you don't want to start with default port 11211, you can change port with following option.
```
$ ./ememcached start -ememcached port 12345
```
