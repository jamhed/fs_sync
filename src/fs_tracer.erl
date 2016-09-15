-module(fs_tracer).
-behaviour(gen_server).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).
-export([go/0, add/2, del/2, clear/0, list/0]).

-record(state, {pid, trace}).

go() ->
	gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

add(M, F) ->
	gen_server:call(?MODULE, {add, M, F}).
clear() ->
	gen_server:call(?MODULE, {clear}).
del(M,F) ->
	gen_server:call(?MODULE, {del, M, F}).
list() ->
	gen_server:call(?MODULE, {list}).

handle_info({trace_ts, _Sender, call, {M,F,A}, _TS}, S=#state{}) ->
	io:format("TRACE: ~s:~s(~p)~n", [M,F,A]),
	{noreply, S};
handle_info(Msg, S=#state{}) ->
	io:format("TRACE ALL:~p~n", [Msg]),
	{noreply, S}.

init([]) ->
	{ok, #state{pid=self(), trace=[]}}.

handle_cast(_Msg, S=#state{}) -> {noreply, S}.

handle_call({add, M, F}, _From, S=#state{pid=Pid, trace=Trace}) ->
	case lists:member({M,F}, Trace) of
		true ->
			{reply, 0, S};
		false ->
			Num = add_trace(Pid, M, F),
			{reply, Num, S#state{trace=[{M,F} | Trace]}}
	end;
handle_call({del, M, F}, _From, S=#state{pid=Pid, trace=Trace}) ->
	case lists:member({M,F}, Trace) of
		true ->
			Num = del_trace(Pid, M, F),
			{reply, Num, S#state{trace=lists:filter(fun(X) -> X =/= {M,F} end, Trace)}};
		false ->
			{reply, 0, S}
	end;
handle_call({clear}, _From, S=#state{pid=Pid, trace=Trace}) ->
	Re = [ del_trace(Pid, M, F) || {M,F} <- Trace ],
	{reply, Re, S#state{trace=[]}};
handle_call({list}, _From, S=#state{trace=Trace}) ->
	{reply, Trace, S};
handle_call(_Request, _From, S=#state{}) -> {reply, ok, S}.
terminate(_Reason, _S) -> ok.
code_change(_OldVsn, S=#state{}, _Extra) -> {ok, S}.

del_trace(Pid, M, F) ->
	erlang:trace_pattern({M,F,'_'}, false, [{meta, Pid}]).

add_trace(Pid, M, F) ->
	erlang:trace(all, true, [call, timestamp]),
	Pattern = [{
		'$1',
		[],
		[return_trace]
	}],
	erlang:trace_pattern({M,F,'_'}, Pattern, [{meta, Pid}]).