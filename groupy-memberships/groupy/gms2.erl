-module(gms2).
-export([start/1, start/2]).

-define(timeout, 2000).
-define(arghh, 100).

% Start this node as a master
start(Id) ->
    Self = self(),
    Rnd = random:uniform(1000),
    {ok, spawn_link(fun()-> init(Id, Rnd, Self) end)}.

init(Id,Rnd, Master) ->
    random:seed(Rnd, Rnd, Rnd),
    leader(Id, Master, [], [Master]).

% Code for acting out the leadership role
leader(Id, Master, Slaves, Group) ->
    receive
        {mcast, Msg} ->
            bcast(Id, {msg, Msg}, Slaves),
            Master ! Msg,
            leader(Id, Master, Slaves, Group);
        {join, Wrk, Peer} ->
            Slaves2 = lists:append(Slaves, [Peer]),
            Group2 = lists:append(Group, [Wrk]),
            bcast(Id, {view, [self()|Slaves2], Group2}, Slaves2),
            Master ! {view, Group2},
            leader(Id, Master, Slaves2, Group2);
        stop ->
            ok
    end.

bcast(Id, Msg, Slaves) ->
    lists:foreach(fun(Slave) -> Slave ! Msg, crash(Id) end, Slaves).

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
        {view, [Leader|Slaves], Group} ->
        Master ! {view, Group},
        erlang:monitor(process, Leader),
        slave(Id, Master, Leader, Slaves, Group)
    after ?timeout ->
        Master ! {error, "no reply from leader"}
    end.

% Slave state code
slave(Id, Master, Leader, Slaves, Group) ->
    receive
        {mcast, Msg} ->
            Leader ! {mcast, Msg},
            slave(Id, Master, Leader, Slaves, Group);
        {join, Wrk, Peer} ->
            Leader ! {join, Wrk, Peer},
            slave(Id, Master, Leader, Slaves, Group);
        {msg, Msg} ->
            Master ! Msg,
            slave(Id, Master, Leader, Slaves, Group);
        {view, [Leader|Slaves2], Group2} ->
            Master ! {view, Group2},
            slave(Id, Master, Leader, Slaves2, Group2);
        {'DOWN', _Ref, process, Leader, _Reason} ->
            election(Id, Master, Slaves, Group);
        stop ->
            ok
    end.

election(Id, Master, Slaves, [_|Group]) ->
    Self = self(),
    case Slaves of
        [Self|Rest] ->
            bcast(Id, {view, Slaves, Group}, Rest),
            Master ! {view, Group},
            leader(Id, Master, Rest, Group);
        [Leader|Rest] ->
            erlang:monitor(process, Leader),
            slave(Id, Master, Leader, Rest, Group)
    end.
