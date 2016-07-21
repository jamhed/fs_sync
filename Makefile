devel:
	rm -rf _rel
	rebar get-deps
	rebar compile
	relx -d
	cp -a cfg _rel/fs_sync

console: devel
	_rel/fs_sync/bin/fs_sync console
