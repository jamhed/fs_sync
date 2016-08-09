-module(fs_sync_event).
-behaviour(gen_event).
-include_lib("fs_sync/include/logger.hrl").
-export([init/1, handle_event/2, terminate/2, handle_info/2, handle_call/2, code_change/3]).

init(_Args) ->
	{ok, []}.

handle_event({Event, File}, State) when Event == modify; Event == rename ->
	handle_type(file_type:detect(File), File),
	{ok, State};
handle_event(_Event, State) ->
	% ?INFO("skip event: ~180p", [_Event]),
	{ok, State}.

terminate(_Args, _State) ->
	ok.

handle_call(_Req, S) -> {ok, ok, S}.
handle_info(_Req, S) -> {ok, S}.
code_change(_OldVsn, S, _Extra) -> {ok, S}.

handle_type(Type, File) ->
	try
		case fs_sync_cfg:is_skip(Type) of
			true -> skip;
			_ ->
				type_handler:handle({Type, File}),
				type_handler:external_handler(fs_sync_cfg:after_handler(), Type, File)
		end
	catch
		Error:Class ->
			?ERR("error handling ~120p ~120p:~120p file:~120p", [Type, Class, Error, File])
	end.
