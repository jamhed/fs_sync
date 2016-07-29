-module(type_handler).
-export([handle/1, explode_path/2]).
-include_lib("scl/include/logger.hrl").

handle({beam, File}) ->
	Module = erlang:list_to_atom(filename:basename(File, ".beam")),
	{Module, Binary, Filename} = code:get_object_code(Module),
	code:load_binary(Module, Filename, Binary),
	?INFO("module '~p' replaced by beam file.", [Module]),
	ok;
handle({erl, File}) ->
	Module = erlang:list_to_atom(filename:basename(File, ".erl")),
	ModuleProps = Module:module_info(compile),
	Source = proplists:get_value(source, ModuleProps),
	Options = transform_options(Module, Source, proplists:get_value(options, ModuleProps)),
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

transform_option(Module, _, {outdir, _Outdir}) ->
	{outdir, filename:dirname(code:which(Module))};
transform_option(Module, Source, {i, Include}) ->
	case filelib:is_dir(Include) of
		true -> {i, Include};
		false -> guess_include(filename:dirname(code:which(Module)), filename:dirname(Source), Include)
	end;
transform_option(_, _, Opt) -> Opt.

transform_options(Module, Source, Options) ->
	lists:foldl(
		fun(Opt, Acc) ->
			[ transform_option(Module, Source, Opt) | Acc ]
		end, [], Options).

guess_include(ModulePath, SourcePath, Include) ->
	case explode_path(ModulePath, Include) ++ explode_path(SourcePath, Include) of
		[] -> {i, Include};
		[Candidate|_] -> {i, Candidate}
	end.

explode_path(Path, Ex) -> 
	[ P || P <- explode_parts(lists:reverse(filename:split(Path)), Ex), filelib:is_dir(P) ].

explode_parts([], _Ex) -> [];
explode_parts([_Part|Parts], Ex) ->
	[ filename:join(lists:reverse([Ex|Parts])) | explode_parts(Parts, Ex) ].

%% formatters

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

