devel:
	rm -rf _build
	rebar3 compile

console: 
	rebar3 release
	_build/default/rel/fs_sync/bin/fs_sync console
