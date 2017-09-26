-module(routy).
-export([start/2, stop/1, script/0, update_all/0, connect_all/0]).

-define(MyIP, 'sweden@130.229.161.98').

start(Reg, Name) ->
	register(Reg, spawn(fun() -> init(Name) end)).


script() ->
	start(sweden, sweden),
	start(newMoskov, newMoskov),
	start(kiruna, kiruna),
	start(kalmar, kalmar),
	start(stockholm, stockholm)
	.

connect_all() ->
	sweden ! {add, newMoskov, {newMoskov, ?MyIP}},
	timer:sleep(100),
	newMoskov ! {add, sweden, {sweden, ?MyIP}},
	timer:sleep(100),

	newMoskov ! {add, kalmar, {kalmar, ?MyIP}},
	timer:sleep(100),
	kalmar ! {add, newMoskov, {newMoskov, ?MyIP}},
	timer:sleep(100),

	newMoskov ! {add, stockholm, {stockholm, ?MyIP}},
	timer:sleep(100),
	stockholm ! {add, newMoskov, {newMoskov, ?MyIP}},
	timer:sleep(100),

	newMoskov ! {add, kiruna, {kiruna, ?MyIP}},
	timer:sleep(100),
	kiruna ! {add, newMoskov, {newMoskov, ?MyIP}}.
	

update_all() ->
	sweden ! update,
	newMoskov ! update,
	kiruna ! update,
	kalmar ! update,
	stockholm ! update.


stop(Node) ->
	Node ! stop,
	unregister(Node).


init(Name) ->
	Intf 	= interfaces:new(),
	Map 	= map:new(),
	Table 	= dijkstra:table(Intf, Map),
	Hist 	= history:new(Name),
	router(Name, 0, Hist, Intf, Table, Map).


router(Name, N, Hist, Intf, Table, Map) ->
	receive
		{links, Node, R, Links} ->
			io:format("~s: ~w~n", [Name, Links]),
			case history:update(Node, R, Hist) of
				{new, Hist1} ->
					interfaces:broadcast({links, Node, R, Links}, Intf),
					Map1 = map:update(Node, Links, Map),
					router(Name, N, Hist1, Intf, Table, Map1);
				old ->
					router(Name, N, Hist, Intf, Table, Map)
			end;

		{add, Node, Pid} ->
			Ref = erlang:monitor(process,Pid),
			Intf1 = interfaces:add(Node, Ref, Pid, Intf),
			io:format("~w=>~w~n", [Intf, Intf1]),
			router(Name, N, Hist, Intf1, Table, Map);
		{remove, Node} ->
			{ok, Ref} = interfaces:ref(Node, Intf),
			erlang:demonitor(Ref),
			Intf1 = interfaces:remove(Node, Intf),
			router(Name, N, Hist, Intf1, Table, Map);
		{'DOWN', Ref, process, _, _} ->
			{ok, Down} = interfaces:name(Ref, Intf),
			io:format("~w: exit recived from ~w~n", [Name, Down]),
			Intf1 = interfaces:remove(Down, Intf),
			router(Name, N, Hist, Intf1, Table, Map);

		% Routing
		{route, Name, From, Message} ->
			io:format("~w: received message ~w from ~w~n", [Name, Message, From]),
			router(Name, N, Hist, Intf, Table, Map);

		{route, To, From, Message} ->
			io:format("~w: routing message (~w)~n", [Name, Message]),
			case dijkstra:route(To, Table) of
				{ok, Gw} ->
					case interfaces:lookup(Gw, Intf) of
						{ok, Pid} ->
							Pid ! {route, To, From, Message};
						notfound ->
							ok
					end;
				notfound ->
					ok
			end,
			router(Name, N, Hist, Intf, Table, Map);

		{send, To, Message} ->
			self() ! {route, To, Name, Message},
			router(Name, N, Hist, Intf, Table, Map);

		% Local commands
		{status, From} ->
			From ! {status, {Name, N, Hist, Intf, Table, Map}},
			router(Name, N, Hist, Intf, Table, Map);

		update ->
			Table1 = dijkstra:table(interfaces:list(Intf), Map),
			router(Name, N, Hist, Intf, Table1, Map);

		broadcas ->
			Message = {links, Name, N, interfaces:list(Intf)},
			interfaces:broadcast(Message, Intf),
			router(Name, N+1, Hist, Intf, Table, Map);

		stop ->
			ok
	end.



