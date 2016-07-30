-module(fs_sync).
-behaviour(gen_server).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).
-include_lib("fs_sync/include/logger.hrl").
-export([go/1, go/0, stop/1]).

-record(state, {path, fs}).

go(Path) -> gen_server:start_link(?MODULE, [Path], []).
go() ->
	{ok, Path} = file:get_cwd(),
	Guess = guess(Path, string:str(Path, "_rel")),
	?INFO("Guessed path: ~s", [Guess]),
	go(Guess).

guess(Path, N) when N > 2 -> string:left(Path, N-2);
guess(Path, _) -> Path.

stop(Pid) -> gen_server:cast(Pid, {stop}).

%% API impl

init([Path]) ->
	{ok, Fs} = fs_event:start_link(Path),
	fs_event:add_handler(Fs, fs_sync_event, [Path]),
	{ok, #state{path=Path, fs=Fs}}.

handle_cast({stop}, S=#state{path=Path, fs=Fs}) ->
	fs_event:delete_handler(Fs, fs_sync_event, [Path]),
	fs_event:stop(Fs),
	{noreply, S};
handle_cast(_Msg, S=#state{}) -> {noreply, S}.

terminate(_Reason, _S) -> ok.

handle_info(_Info, S=#state{}) -> {noreply, S}.
handle_call(_Request, _From, S=#state{}) -> {reply, ok, S}.
code_change(_OldVsn, S=#state{}, _Extra) -> {ok, S}.
