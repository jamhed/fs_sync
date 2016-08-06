-module(file_type).
-export([detect/1]).

detect(File) ->
	detect_by_ext(filename:extension(File)).

detect_by_ext("") -> "no_extenstion";
detect_by_ext(Ext) -> string:sub_string(Ext, 2).
