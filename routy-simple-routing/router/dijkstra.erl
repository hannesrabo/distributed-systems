-module(dijkstra).
-export([entry/2, update/4, iterate/3]).

% Returns lenght of shortest path to Node or 0

% If we didn't find it during iteration
entry(_Node, []) ->
	0;
% If the first element in the sorted list is infinitely far away, our element is unreachable.
entry(_Node, [{_ToNode, inf, _Gateway}, _Sorted] ) ->
	0;
% If we found the shortest path
entry(Node, [{Node, ShortestPath, _Gateway} | _Sorted]) ->
	ShortestPath;
% If this is not the element we are looking for
entry(Node, [_Head | Sorted]) ->
	entry(Node, Sorted).

% Check if this is a shorter path than we have, if so replace current
% If no path is found, nothing happeneds
update(Node, N, Gateway, Sorted) ->
	PathLength = entry(Node, Sorted),
	if
		N < PathLength ->
			lists:keysort(2, replace(Node, N, Gateway, Sorted));
		true ->
			Sorted
	end.

% Return a new sorted list where node has been updated. 

% If we hit the end
replace(_Node, _N, _Gateway, []) ->
	[];
% If we found our node
replace(Node, N, Gateway, [{Node, _PathLength, _Gateway} | Sorted]) ->
	[{Node, N, Gateway} | Sorted];
% If this is still not our node
replace(Node, N, Gateway, [Head | Sorted]) ->
	[Head | replace(Node, N, Gateway, Sorted) ].

% Creates the routing table. We need to reverse it to get the shortest path first 
% as the list is accumulated
iterate([], _Map, Table) ->
	lists:reverse(Table);

iterate([{_Node, inf, _LinkNode} | _Rest], _Map, Table) ->
	lists:reverse(Table);

iterate([{ToNode, N, GatewayNode} | Sorted], Map, Table) ->
	% For each of the reachable nodes from the linked node, update the list of entries.
	% This returns the new updated sorted list.
	iterate(
		lists:foldl( 
			fun(ReachableNode, Acc) ->
				% Add the reachable node as a possible path to current gateway (startnode)
				update( ReachableNode, N + 1, GatewayNode, Acc)
			end,
			Sorted, 					% Use the prev. sorted list as the accumulator
			map:reachable(ToNode, Map) 	% Iterate through the new available nodes
		),
		Map,
		[{ToNode, GatewayNode} | Table ] 	% This is currently the shortest new path
	).	
