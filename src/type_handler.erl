-module(type_handler).
-export([handle/1]).
-include_lib("scl/include/logger.hrl").

handle({beam, File}) ->
	?INFO("beam: ~s", [File]),
	Module = erlang:list_to_atom(filename:basename(File, ".beam")),
	{Module, Binary, Filename} = code:get_object_code(Module),
	code:load_binary(Module, Filename, Binary),
	?INFO("module '~p' replaced by beam file.", [Module]),
	ok;
handle({erl, File}) ->
	?INFO("erl: ~s", [File]),
	Module = erlang:list_to_atom(filename:basename(File, ".erl")),
	Options = Module:module_info(compile),
	handle_erl_compile(Module, compile:file(filename:rootname(File), [return|Options])),
	ok;
handle({undefined, _File}) ->
	ok;
handle({Type, File}) ->
	?INFO("unhandled file:~p type:~p", [File, Type]),
	ok.

handle_erl_compile(Module, {ok, Module, []}) ->
	?INFO("'~p' recompiled.", [Module]);
handle_erl_compile(Module, {ok, Module, Warnings}) ->
	?WARN("'~p' recompiled with warnings.~n~n~s", [Module, lists:flatten(format_errors([], Warnings))]);
handle_erl_compile(Module, {error, Errors, Warnings}) ->
	?ERR("'~p' compile errors:~n~n~s", [Module, lists:flatten(format_errors(Errors, Warnings))]).

format_errors(Errors, Warnings) ->
	AllErrors1 = lists:sort(lists:flatten([X || {_, X} <- Errors])),
	AllErrors2 = [{Line, "Error", Module, Description} || {Line, Module, Description} <- AllErrors1],
	AllWarnings1 = lists:sort(lists:flatten([X || {_, X} <- Warnings])),
	AllWarnings2 = [{Line, "Warning", Module, Description} || {Line, Module, Description} <- AllWarnings1],
	Everything = lists:sort(AllErrors2 ++ AllWarnings2),
	F = fun({Line, Prefix, Module, ErrorDescription}) ->
		Msg = format_error(Module, ErrorDescription),
		io_lib:format(".~p: ~s: ~s~n", [Line, Prefix, Msg])
	end,
	[F(X) || X <- Everything].

format_error(Module, ErrorDescription) ->
	case erlang:function_exported(Module, format_error, 1) of
		true -> Module:format_error(ErrorDescription);
		false -> io_lib:format("~s", [ErrorDescription])
	end.
