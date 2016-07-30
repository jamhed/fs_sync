-module(file_type).
-export([detect/1]).

detect(File) ->
	detect_by_ext(filename:extension(File)).

detect_by_ext(".beam") -> beam;
detect_by_ext(".erl") -> erl;
detect_by_ext(".hrl") -> hrl;
detect_by_ext(Ext) -> Ext.