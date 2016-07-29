devel:
	rm -rf _rel
	rebar get-deps
	rebar compile
	relx -d

console: 
	rebar co
	_rel/fs_sync/bin/fs_sync console
