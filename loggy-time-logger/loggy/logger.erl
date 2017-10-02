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
			io:format("~w~n", [length(Queue)]),
			NewQueue = print_safe(NewClock, insert_in_queue(LogMessage, Queue)),
			%io:format("~w~n~w~n", [NewClock, NewQueue]),
			loop(NewClock, NewQueue);
		stop ->
			io:format("~w~n.", [length(Queue)]),
			ok
	end.

print_safe(_Clock, []) ->
	[];
print_safe(Clock, [{log, From, Time, Msg} = LogMessage | Rest]) ->
	case time:safe(Time, Clock) of
		true ->
			log(From, Time, Msg),
			print_safe(Clock, Rest);
		_ ->
			%io:format("unsafe ~n", []),
			[LogMessage | print_safe(Clock, Rest)]
	end.


log(From, {_Name, Time}, Msg) ->
	io:format("[~w] (~w): ~p~n", [Time, From, Msg]).
