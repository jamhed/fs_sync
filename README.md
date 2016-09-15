FS Sync
=======

Monitors file system events using [fs\_event](http://github.com/jamhed/fs_event) and recompiles .erl files on the fly using compile:file/2, reloads beam files by code:load\_binary/3.

Syncer
======
```erlang
fs_sync:start("/cool/erlang/project/").
fs_sync:start()/go().
fs_sync:stop().
fs_sync:watch(Path).
fs_sync:unwatch(Path).
```

fs_sync:go/0 tries to guess project root (e.g. goes two level up from _rel folder).

Tracer
======
```erlang
fs_tracer:go().
fs_tracer:add(Module, Function).
fs_tracer:del(Module, Function).
fs_tracer:list().
fs_tracer:clear().
```

Enables tracing for specific functions. Tracing is enabled on all processes.

Sample output:
```erlang
(fs_sync@127.0.0.1)1> fs_tracer:go().
(fs_sync@127.0.0.1)2> fs_tracer:add(lists, seq).
(fs_sync@127.0.0.1)3> spawn(fun() -> X = lists:seq(1,5) end).
TRACE: lists:seq/2 <- [1,5]
TRACE: lists:seq/2 -> [1,2,3,4,5]
```

Please note that when module is reloaded (e.g. with fs_sync), then tracing is resetted.

External handlers
=================

File type is determined by file extension. Natively handled types are "erl" and "beam". It is possible to define
an external type handler for unknown file types as **default\_handler** parameter (script will be executed in erlang node cwd),
and an external handler to execute after internal handler -- **after\_handler** parameter.
External handlers are called with Erlang os:cmd/1, with type and filename provided as command line arguments, see 
[example/handler.sh](example/handler.sh).

Configuration options
=====================

Complete documented configuration file [sys.config](rel/sys.config).

* **skip\_types** -- list of file extensions to ignore completely
* **synthesize\_beam\_event** -- a helper for naive fs_event scanner, to reload beam file immediately after compilation
* **after_handler**  -- see External handlers
* **default_handler** -- see External handlers