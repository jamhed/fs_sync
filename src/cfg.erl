-module(cfg).
-export([synthesize_beam_event/0]).

truefy({ok, true}) -> true;
truefy(_) -> false.

synthesize_beam_event() ->
	truefy(application:get_env(fs_sync, synthesize_beam_event)).
