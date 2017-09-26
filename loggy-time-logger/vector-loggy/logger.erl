-module(logger).
-export([start/1, stop/1]).

start(Nodes) ->
	spawn_link(fun() -> init(Nodes) end).

stop(Logger) ->
	Logger ! stop.

init(Nodes) ->
	loop(time:clock(Nodes), []).

insert_in_queue(LogMessage, []) ->
	[LogMessage];
insert_in_queue({log, _From, TimeIn, _Msg} = LogMessage, [ListHead = {log, _From2, TimeItem, _Msg2}| ListRest] = List) ->
	case time:leq(TimeIn, TimeItem) of
		true ->
			[LogMessage | List];
		_Else ->
			[ListHead | insert_in_queue(LogMessage, ListRest)]
	end.

loop(Clock, Queue) ->
	receive
		{log, From, Time, _Msg} = LogMessage ->
			NewClock = time:update(From, Time, Clock),
			%io:format("~n", []),
			%io:format("~w : ~w~n", [LogMessage, Queue]),
			NewQueue = print_safe(NewClock, insert_in_queue(LogMessage, Queue)),

			loop(NewClock, NewQueue);
		stop ->
			ok
	end.

print_safe(_Clock, []) ->
	[];
print_safe(Clock, [{log, From, Time, Msg} = LogMessage | Rest]) ->
	% io:format("~w ~w~n", [Time, time:safe(Time, Clock)]),
	case time:safe(Time, Clock) of
		true ->
			log(From, Time, Msg),
			print_safe(Clock, Rest);
		_ ->
			%io:format("unsafe ~n", []),
			[LogMessage | print_safe(Clock, Rest)]
	end.


log(From, Time, Msg) ->
	io:format("[~s]	(~w): ~p~n", [time:toString(Time), From, Msg]).
