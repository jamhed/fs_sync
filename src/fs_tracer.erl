-module(fs_tracer).
-behaviour(gen_server).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).
-export([go/0, trace/2]).

-record(state, {pid}).

go() ->
	gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

trace(M, F) ->
	gen_server:cast(?MODULE, {trace, M, F}).

handle_info({trace_ts, _Sender, call, {M,F,A}, _TS}, S=#state{}) ->
	io:format("TRACE: ~s:~s(~p)~n", [M,F,A]),
	{noreply, S};
handle_info(Msg, S=#state{}) ->
	io:format("TRACE ALL:~p~n", [Msg]),
	{noreply, S}.

init([]) ->
	{ok, #state{pid=self()}}.

handle_cast({trace, M, F}, S=#state{pid=Pid}) ->
	erlang:trace(all, true, [call, return_to]),
	Pattern = [{
		'$1',
		[],
		[return_trace]
	}],
	erlang:trace_pattern({M,F,'_'}, Pattern, [{meta, Pid}]),
	{noreply, S};
handle_cast(_Msg, S=#state{}) -> {noreply, S}.

handle_call(_Request, _From, S=#state{}) -> {reply, ok, S}.
terminate(_Reason, _S) -> ok.
code_change(_OldVsn, S=#state{}, _Extra) -> {ok, S}.
