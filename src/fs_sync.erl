-module(fs_sync).
-behaviour(gen_server).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).
-include_lib("fs_sync/include/logger.hrl").
-export([start/0, start/1, start_link/0, start_link/1, stop/0, watch/1, unwatch/1]).

-record(state, {watchers = []}).
-record(watcher, {pid, path}).

start() ->
	gen_server:start({local, ?MODULE}, ?MODULE, [], []).
start(Path) ->
	gen_server:start({local, ?MODULE}, ?MODULE, [Path], []).

start_link() ->
	gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).
start_link(Path) ->
	gen_server:start_link({local, ?MODULE}, ?MODULE, [Path], []).

watch(Path) ->
	gen_server:call(?MODULE, {watch, Path}).
unwatch(Path) ->
	gen_server:call(?MODULE, {unwatch, Path}).

guess(Path, N) when N > 2 -> string:left(Path, N-2);
guess(Path, _) -> Path.

stop() -> gen_server:cast(?MODULE, {stop}).

%% API impl

init([]) ->
	{ok, Path} = file:get_cwd(),
	Guess = guess(Path, string:str(Path, "_rel")),
	{ok, #state{watchers=[start_watcher(Guess)]}};
init([Path]) ->
	{ok, #state{watchers=[start_watcher(Path)]}}.

handle_cast({stop}, S=#state{watchers=Watchers}) ->
	stop_watchers(Watchers),
	{stop, normal, S#state{watchers=[]}};
handle_cast(_Msg, S=#state{}) -> {noreply, S}.

terminate(_Reason, _S) -> ok.

handle_info(_Info, S=#state{}) -> {noreply, S}.

handle_call({watch, Path}, _From, #state{watchers=Watchers}=S) ->
	case find_watcher(Path, Watchers) of
		false ->
			Watcher = start_watcher(Path),
			{reply, {ok, Path}, S#state{watchers=[Watcher | Watchers]}};
		_ ->
			{reply, {exists, Path}, S}
	end;
handle_call({unwatch, Path}, _From, #state{watchers=Watchers}=S) ->
	case find_watcher(Path, Watchers) of
		#watcher{} = Watcher ->
			stop_watchers([Watcher]),
			{reply, {ok, Path}, S#state{watchers=lists:keydelete(Path, #watcher.path, Watchers)}};
		false ->
			{reply, {not_watching, Path}, S}
	end;
handle_call(_Request, _From, S=#state{}) -> {reply, ok, S}.
code_change(_OldVsn, S=#state{}, _Extra) -> {ok, S}.

%% Impl

start_watcher(Path) ->
	?INFO("start watcher for: ~p", [Path]),
	{ok, Fs} = fs_event:start_link(Path),
	fs_event:add_handler(Fs, fs_sync_event, [Path]),
	#watcher{pid=Fs, path=Path}.

find_watcher(Path, Watchers) ->
	lists:keyfind(Path, #watcher.path, Watchers).

stop_watchers([]) -> ok;
stop_watchers([ #watcher{pid=Fs, path=Path} | Watchers ]) ->
	?INFO("stop watcher for: ~p", [Path]),
	fs_event:delete_handler(Fs, fs_sync_event, [Path]),
	fs_event:stop(Fs),
	stop_watchers(Watchers).
