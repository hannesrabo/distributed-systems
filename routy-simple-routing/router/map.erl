-module(map).
-export([new/0, update/3, reachable/2, all_nodes/1]).

% Returns the empty data structure (a Map).
new() ->
	[].


% If this nodes does not match the head of the map: 
% Just append the result of the adding to this node
update (Node, Links, Map) ->
	NewMap = lists:keydelete(Node, 1, Map),
	[{Node, Links} | NewMap].


% Returns the list of nodes reachable from "Node"
reachable(Node, Map) ->
	case lists:keyfind(Node, 1, Map) of
		{_Node, Links} ->
			Links;
		false ->
			[]
	end.

% Returns all nodes in network
all_nodes(Map) ->
	% Create a list from the set we just created.
	sets:to_list( 
		% Create a set of all the relevant elements
		lists:foldl( 
			fun({Node, Links}, Acc) -> 
				% Create a set from the links and the node, add this to the accumulator.
				sets:union(sets:from_list([Node | Links]), Acc)
			end, 
			
			sets:new(), 
			Map
		)
	).



