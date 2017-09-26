-module(dijkstra).
-export([table/2, route/2, iterate/3]).

% Returns lenght of shortest path to Node or 0

% If we didn't find it during iteration
entry(_Node, []) ->
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
			replace(Node, N, Gateway, Sorted);
		true ->
			Sorted
	end.

% Return a new sorted list where node has been updated. 
replace(Node, N, Gateway, Sorted) ->
	 lists:keysort(2, replace_int(Node, N, Gateway, Sorted)). 

% The internal replace function
% If we hit the end
replace_int(_Node, _N, _Gateway, []) ->
	[];
% If we found our node
replace_int(Node, N, Gateway, [{Node, _PathLength, _Gateway} | Sorted]) ->
	[{Node, N, Gateway} | Sorted];
% If this is still not our node
replace_int(Node, N, Gateway, [Head | Sorted]) ->
	[Head | replace(Node, N, Gateway, Sorted) ].

% Creates the routing table. We need to reverse it to get the shortest path first 
% as the list is accumulated
iterate([], _Map, Table) ->
	Table;

iterate([{_Node, inf, _LinkNode} | _Rest], _Map, Table) ->
	Table;

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

table(Gateways, Map) ->
	% Add all nodes in the map with infinite length
	AllNodes = lists:foldl(
					fun(Node, Acc) ->
						[{Node, inf, unknown} | Acc]
					end,
					[],
					map:all_nodes(Map)
				),
	% Iterate over all the sorted list of nodes in the network and create the table.
	iterate(
		% Add the gatewaynodes with 0 path length to the list before we start.
		lists:foldl(
			fun(GatewayNode, Acc) ->
				[{GatewayNode, 0, GatewayNode} | Acc]
			end,
			AllNodes,
			Gateways
		),
		Map, 	% Network map.
		[] 		% The initial empty table
	).

route(Node, Table) ->
	case lists:keyfind(Node, 1, Table) of
		{_Node, Gateway} ->
			{ok, Gateway};
		false ->
			notfound		
	end.

















