-module(trace_fmt).
-export([hide_records/1]).

hide_records(R) when is_tuple(R) ->
	Name = erlang:element(1, R),
	case Name of
		Name when is_atom(Name) -> Name;
		_ -> R
	end.