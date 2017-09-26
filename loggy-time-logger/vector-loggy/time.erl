-module(time).
-export([zero/0, inc/2, merge/2, leq/2, clock/1, update/3, safe/2, toString/1]).

zero() ->
	maps:new().

inc(Name, Time ) ->
	maps:update_with(
		Name,
		fun(Val) ->
			Val + 1 end,
		1,
		Time
	).

merge(Time1, Time2) ->
	NewMap = maps:merge(Time2, Time1),
	maps:map(
		fun(Key, Value) ->
			NewValue = maps:get(Key, Time2, 0),
			case Value < NewValue of
				true ->
					NewValue;
				_Else ->
					Value
			end
		end,
		NewMap
	).

leq(Time1, Time2) ->
	maps:fold(
		fun(Key, Value, Acc) ->
			%io:format("~w~w~w~n", [Value, maps:get(Key, Time2, 0), (Value =< maps:get(Key, Time2, 0))]),
			((Value =< maps:get(Key, Time2, 0)) and Acc)
		end,
		true,
		Time1
	).



toString(Time) ->
	String = maps:fold(
		fun(_Key, Val, Acc) ->
			Acc ++ integer_to_list(Val) ++ " "
		end,
		"<",
		Time
	),
	String ++ ">".



clock(Nodes) ->
	lists:foldl(
		fun(Node, Acc) ->
			maps:put(Node, zero(), Acc)
		end,
		maps:new(),
		Nodes
	).

update(Node, Time, Clock) ->
	case maps:find(Node, Clock) of
		{ok, Value} ->
			case leq(Value, Time) of
				true ->
					maps:put(Node, Time, Clock);
				_ ->
					Clock

			end;
		error ->
			maps:put(Node, Time, Clock)
	end.



% The only way that we can know for sure with this implementation
% is if this is the lowest value in the entire map
safe(Time, Clock) ->
	maps:fold(
		fun(_Key, NodeTime, Acc) ->
			%io:format("~w ~w ~w ~n", [Time, NodeTime, leq(Time, NodeTime)]),
			(leq(Time, NodeTime)) and (Acc)
		end,
		true,
		Clock
	).
