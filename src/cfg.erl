-module(cfg).
-export([
	synthesize_beam_event/0,
	default_handler/0,
	after_handler/0,
	is_skip/1
]).

-define(APP, fs_sync).

truefy({ok, Value}) -> Value;
truefy(_) -> false.

synthesize_beam_event() -> truefy(application:get_env(?APP, synthesize_beam_event)).
default_handler() -> truefy(application:get_env(?APP, default_handler)).
after_handler() -> truefy(application:get_env(?APP, after_handler)).
skip_types() -> truefy(application:get_env(?APP, skip_types)).

is_skip(Type) ->
	is_skip(Type, skip_types()).

is_skip(Type, Types) when is_list(Types) -> lists:member(Type, Types);
is_skip(_, _) -> false.
