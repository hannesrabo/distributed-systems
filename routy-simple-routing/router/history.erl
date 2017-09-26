-module(history).
-export([new/1, update/3]).

% Creates a new history map where every entry for this name is always seen as old
new(Name) ->
	maps:put(Name, old, maps:new()).

update(Node, N, History) ->
	case maps:find(Node, History) of
		% If a previous entry exists
		{ok, Value} ->
			if 
				% But it is lower than current: ok
				% The comparison between an atom and integer will always say that the atom is bigger!
				N > Value ->
					{new, maps:put(Node, N, History)};

				% And it is higher: This is old!
				true ->
					old
			end;

		% If no previous entry exists: It can not be old!
		error ->
			{new, maps:put(Node, N, History)}
	end.
