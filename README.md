FS Sync
=======

Monitor events from fs\_event. Recompile .erl files on the fly using compile:file/2, reload beam files by code:load\_binary/3.

Usage
=====
```erlang
fs_sync:go("/cool/erlang/project/").
fs_sync:go().
```

fs_sync:go/0 tries to guess project root (e.g. goes two level up from _rel folder).

TODO
====

1. More docs and usage examples
2. Speed-up naive beam reloader (notify beam is ready after compilation)
3. Configuration parameters: naive reloader -- rescan period, follow/skip symlinks.
4. configure external commands as "default" handlers per file type (e.g. rebar co, make, ...)
5. configure external commands to call as extra after internal compiler/reloader 