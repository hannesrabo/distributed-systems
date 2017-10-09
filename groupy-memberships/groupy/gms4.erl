-module(gms4).
-export([start/1, start/2]).

-define(timeout, 1000).
-define(arghh, 100).

% Start this node as a master
start(Id) ->
    Self = self(),
    Rnd = random:uniform(1000),
    {ok, spawn_link(fun()-> init(Id, Rnd, Self) end)}.

init(Id, Rnd, Master) ->
    random:seed(Rnd, Rnd, Rnd),
    leader(Id, Master, 0, 0, [], [Master]).

% Code for acting out the leadership role
leader(Id, Master, N_msg, N_view, Slaves, Group) ->
    receive
        {mcast, Msg} ->
            bcast(Id, {msg, N_msg, Msg}, Slaves),
            Master ! Msg,
            leader(Id, Master, N_msg + 1, N_view, Slaves, Group);
        {state_update, Peer} ->
            % This is a invisible update. No increase of number
            Peer ! {view_update, N_msg, N_view, Group},
            io:format("Send view update!~n"),
            leader(Id, Master, N_msg, N_view, Slaves, Group);
        {join, Wrk, Peer} ->
            Slaves2 = lists:append(Slaves, [Peer]),
            Group2 = lists:append(Group, [Wrk]),
            bcast(Id, {view, N_msg, N_view, [self()|Slaves2], Group2}, Slaves2),
            Master ! {view, Group2},
            leader(Id, Master, N_msg, N_view + 1, Slaves2, Group2);
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
        {view, N_msg, N_view, [Leader|Slaves], Group} ->
            Master ! {view, Group},
            erlang:monitor(process, Leader),
            io:format("~w has joined~n", [Id]),
            slave(Id, Master, Leader, -1, -1, {msg, -1, "node not initialized"}, Slaves, Group)
    after ?timeout ->
        io:format("[~w] Did not receive any view~n", [Id]),
        Master ! {error, "no reply from leader"}
    end.

% Slave state code
slave(Id, Master, Leader, N_msg, N_view, Last, Slaves, Group) ->
    % io:format("Slave: ~w~n", [Id]),
    receive
        {mcast, Msg} ->
            Leader ! {mcast, Msg},
            slave(Id, Master, Leader, N_msg, N_view, Last, Slaves, Group);
        {join, Wrk, Peer} ->
            Leader ! {join, Wrk, Peer},
            io:format("[~w] Routing join request~n", [Id]),
            slave(Id, Master, Leader, N_msg, N_view, Last, Slaves, Group);

        {msg, I, Msg} when (I =< N_msg) ->
            dropped,
            io:format("Dropped msg: ~w~n", [Msg]),
            slave(Id, Master, Leader, N_msg, N_view, Last, Slaves, Group);
        {msg, I, Msg} = NewLast when (I == (N_msg + 1)) ->
            io:format("[~w]: ~w~n", [Id, NewLast]),
            Master ! Msg,
            slave(Id, Master, Leader, I, N_view, NewLast, Slaves, Group);
        % Slave has outdated state. Needs to receive it again.
        {msg, _I, _Msg} ->
            % perhaps we need to clear the channel?
            io:format("Outdated state~n"),
            Leader ! {state_update, self()},
            slave(Id, Master, Leader, N_msg, N_view, Last, Slaves, Group);
        {view_update, N_msg_new, N_view_new, NewGroup} ->
            Master ! {view, NewGroup},
            io:format("Updated view~n"),
            slave(Id, Master, Leader, N_msg_new, N_view_new, Last, Slaves, NewGroup);
        {view, I, _, _} when (I =< N_view)->
            io:format("Dropped View~n"),
            slave(Id, Master, Leader, N_msg, N_view, Last, Slaves, Group);
        {view, I, [Leader|Slaves2], Group2} = NewLast ->
            Master ! {view, Group2},
            slave(Id, Master, Leader, N_msg, I, NewLast, Slaves2, Group2);

        {'DOWN', _Ref, process, Leader, _Reason} ->
            election(Id, Master, N_msg, N_view, Last, Slaves, Group);
        stop ->
            ok;
        Msg ->
            io:format("Malformed message: ~w~n", [Msg]),
            slave(Id, Master, Leader, N_msg, N_view, Last, Slaves, Group)
    end.

election(Id, Master, N_msg, N_view, Last, Slaves, [_|Group]) ->
    Self = self(),
    case Slaves of
        [Self|Rest] ->
            io:format("~w became the new master: ~w~n", [Id, Rest]),
            bcast(Id, Last, Rest),
            bcast(Id, {view, N_msg, N_view + 1, Slaves, Group}, Rest),
            Master ! {view, Group},
            leader(Id, Master, N_msg, N_view + 2, Rest, Group);
        [Leader|Rest] ->
            erlang:monitor(process, Leader),
            io:format("~w became a new slave~n", [Id]),
            slave(Id, Master, Leader, N_msg, N_view, Last, Rest, Group)
    end.
