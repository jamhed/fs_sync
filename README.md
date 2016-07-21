FS Sync
=======

Monitor events from fs\_event. Recompile .erl files on the fly using compile:file/2, reload beam files by code:load\_binary/3.

Usage
=====
```erlang
fs_sync:start("/cool/erlang/project/").
```

TODO
====

1. More docs and usage examples
2. Add naive native filesystem monitor (a-la sync)
