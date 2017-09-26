-module(logger).
-export([start/1, stop/1]).

start(Nodes) ->
	spawn_link(fun() -> init(Nodes) end).

stop(Logger) ->
	Logger ! stop.

init(Nodes) ->
	loop(time:clock(Nodes), []).

loop(Clock, Queue) ->
	receive 
		{log, From, Time, _Msg} = LogMessage ->
			NewClock = time:update(From, Time, Clock),
			NewQueue = print_safe(NewClock, Queue ++ [LogMessage]),
			%io:format("~w~n~w~n", [NewClock, NewQueue]),
			loop(NewClock, NewQueue);
		stop ->
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

