FS Sync
=======

Monitor events from fs\_event. Recompile .erl files on the fly using compile:file/2, reload beam files by code:load\_binary/3.
Relies on FS Event application for file system events.

Usage
=====
```erlang
fs_sync:start("/cool/erlang/project/").
fs_sync:start().
fs_sync:stop().
fs_sync:watch(Path).
fs_sync:unwatch(Path).
```

fs_sync:go/0 tries to guess project root (e.g. goes two level up from _rel folder).

External handlers
=================

File type is determined by file extension. Natively handled types are "erl" and "beam". It is possible to define
an external type handler for unknown file types as *default_handler* parameter (script will be executed in erlang node cwd),
and an external handler to execute after internal handler -- *after_handler* parameter.
External handlers will be called with Erlang os:cmd/1, with type and filename provided as command line arguments, see *example/handler.sh*.

Configuration options
=====================

*skip_types* -- list of file extensions to ignore completely
*synthesize_beam_event* -- a helper for naive fs_event scanner, to reload beam file immediately after compilation