-module(fs_sync).
-behaviour(gen_server).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).
-include_lib("scl/include/logger.hrl").
-export([start_link/1]).

-record(state, {}).

start_link(Path) -> gen_server:start_link(?MODULE, [Path], []).

%% API impl

init([Path]) ->
	{ok, Fs} = fs_event:start_link(Path),
	fs_event:add_handler(Fs, fs_sync_event, [Path]),
	{ok, #state{}}.
handle_cast(_Msg, S=#state{}) -> {noreply, S}.

handle_info(_Info, S=#state{}) ->
	?INFO("info: ~p", [_Info]),
	{noreply, S}.
handle_call(_Request, _From, S=#state{}) -> {reply, ok, S}.

terminate(_Reason, _S) -> ok.
code_change(_OldVsn, S=#state{}, _Extra) -> {ok, S}.