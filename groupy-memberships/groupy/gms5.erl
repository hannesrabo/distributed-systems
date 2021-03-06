-module(gms5).
-export([start/1, start/2]).

-define(timeout, 5000).
-define(arghh, 100).
-define(arghh2, 50).

% Start this node as a master
start(Id) ->
    Self = self(),
    Rnd = random:uniform(1000),
    {ok, spawn_link(fun()-> init(Id, Rnd, Self) end)}.

init(Id,Rnd, Master) ->
    random:seed(Rnd, Rnd, Rnd),
    leader(Id, Master, 0, [], [Master]).

% Code for acting out the leadership role
leader(Id, Master, N, Slaves, Group) ->
    receive
        {mcast, Msg} ->
            bcast(Id, {msg, N, Msg}, Slaves),
            Master ! Msg,
            leader(Id, Master, N + 1, Slaves, Group);
        {join, Wrk, Peer} ->
            Slaves2 = lists:append(Slaves, [Peer]),
            Group2 = lists:append(Group, [Wrk]),
            bcast(Id, {view, N, [self()|Slaves2], Group2}, Slaves2),
            Master ! {view, Group2},
            leader(Id, Master, N + 1, Slaves2, Group2);
        stop ->
            ok
    end.

bcast(Id, Msg, Slaves) ->
    lists:foreach(
        fun(Slave) ->
            crashSend(Id, Slave, Msg),
            crash(Id)
        end,
    Slaves),
    acknoledgeMessage(Id, Msg, Slaves, length(Slaves)).

acknoledgeMessage(_Id, Msg, _Slaves, 0) ->
    io:format("acced: ~w~n", [Msg]),
    ok;
acknoledgeMessage(Id, Msg, Slaves, NrOfAcs) ->
    io:format("Trying: ~w nr left ~w ~n", [Msg, NrOfAcs]),
    receive
        {acc, Msg} ->
            acknoledgeMessage(Id, Msg, Slaves, NrOfAcs - 1)
    after 1000 ->
        bcast(Id, Msg, Slaves)
    end.

crashSend(Id, To, Msg) ->
    case random:uniform(?arghh2) of
        ?arghh2 ->
            io:format("leader ~w: dropped message~n", [Id]);
        _ ->
            To ! Msg
    end.

crash(Id) ->
    case random:uniform(?arghh) of
        ?arghh ->
            io:format("leader ~w: crash~n", [Id]),
            exit(no_luck);
        _ ->
            ok
    end.

% Start process for a slave
start(Id, Grp) ->
    Self = self(),
    Rnd = random:uniform(1000),
    {ok, spawn_link(fun()-> init(Id, Rnd, Grp, Self) end)}.

% Initialization for slave process
init(Id, Rnd, Grp, Master) ->
    Self = self(),
    Grp ! {join, Master, Self},
    random:seed(Rnd, Rnd, Rnd),
    receive
        {view, N, [Leader|Slaves], Group} ->
            Master ! {view, Group},
            erlang:monitor(process, Leader),
            io:format("~w has joined~n", [Id]),
            slave(Id, Master, Leader, N, {msg, N-1, "node not initialized"}, Slaves, Group)
    after ?timeout ->
        io:format("[~w] Did not receive any view~n", [Id]),
        Master ! {error, "no reply from leader"}
    end.

% Slave state code
slave(Id, Master, Leader, N, Last, Slaves, Group) ->
    % io:format("Slave: ~w~n", [Id]),
    receive
        {mcast, Msg} ->
            Leader ! {mcast, Msg},
            slave(Id, Master, Leader, N, Last, Slaves, Group);
        {join, Wrk, Peer} ->
            Leader ! {join, Wrk, Peer},
            io:format("[~w] Routing join request", [Id]),
            slave(Id, Master, Leader, N, Last, Slaves, Group);
        {msg, I, _Msg} = NewLast when (I =< N) ->
            dropped,
            io:format("Dropped msg~n"),
            Leader ! {acc, NewLast},
            slave(Id, Master, Leader, N, Last, Slaves, Group);
        {msg, I, Msg} = NewLast ->
            io:format("[~w]: ~w~n", [Id, NewLast]),
            Leader ! {acc, NewLast},
            Master ! Msg,
            slave(Id, Master, Leader, I, NewLast, Slaves, Group);
        {view, I, _, _} = NewLast when (I =< N)->
            io:format("Dropped View~n"),
            Leader ! {acc, NewLast},
            slave(Id, Master, Leader, N, Last, Slaves, Group);
        {view, I, [Leader|Slaves2], Group2} = NewLast ->
            Leader ! {acc, NewLast},
            io:format("Acc: ~w~n", [NewLast]),
            Master ! {view, Group2},
            slave(Id, Master, Leader, I, NewLast, Slaves2, Group2);
        {'DOWN', _Ref, process, Leader, _Reason} ->
            election(Id, Master, N, Last, Slaves, Group);
        stop ->
            ok
    end.

election(Id, Master, N, Last, Slaves, [_|Group]) ->
    Self = self(),
    case Slaves of
        [Self|Rest] ->
            io:format("~w became the new master: ~w~n", [Id, Rest]),
            bcast(Id, Last, Rest),
            bcast(Id, {view, N + 1, Slaves, Group}, Rest),
            Master ! {view, Group},
            leader(Id, Master, N + 2, Rest, Group);
        [Leader|Rest] ->
            erlang:monitor(process, Leader),
            io:format("~w became a new slave~n", [Id]),
            slave(Id, Master, Leader, N, Last, Rest, Group)
    end.
