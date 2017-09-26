-module(time).
-export([zero/0, inc/2, merge/2, leq/2, clock/1, update/3, safe/2]).

zero() ->
	{noname, 0}.

inc(Name, {_OldName, T} ) ->
	{Name, T + 1}.

merge({_, Ti}, {_, Tj} = A2) when (Ti < Tj)->
	A2;
merge(A1, _A2) ->
	A1.

leq({_, Ti}, {_, Tj}) when (Ti =< Tj)->
	true;
leq(_A1, _A2) ->
	false.
	


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
