-module(node3).
-export([start/1, start/2]).

-define(StabilizeTimeout, 1000).
-define(ConnectionTimeout, 5000).

% Initialization
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
start(Id) ->
    start(Id, nil).

start(Id, Peer) ->
    timer:start(),
    spawn(fun() -> init(Id, Peer) end).

init(Id, Peer) ->
    Predecessor = nil,
    {ok, {Skey, Spid}} = connect(Id, Peer),
    Sref = monitor(Spid),
    % io:format("Successfully connected~n"),
    schedule_stabilize(),
    % TODO: this is a problem? (nil)
    node(Id, Predecessor, {Skey, Sref, Spid}, storage:create(), nil).

connect(Id, nil) ->
    {ok, {Id, self()}};
connect(Id, Peer) ->
    Qref = make_ref(),
    Peer ! {key, Qref, self()},
    receive
        {Qref, Skey} ->
            {ok, {Skey, Peer}}
    after ?ConnectionTimeout ->
        io:format("[~w] Time out: no response~n",[Id])
    end.


% Main message handler
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
node(Id, Predecessor, Successor, Store, Next) ->
    % io:format("[~w] P: ~w, S: ~w St: ~w~n", [Id, Predecessor, Successor, Store]),
    receive
        {key, Qref, Peer} ->
            % io:format("1~n"),
            Peer ! {Qref, Id},
            node(Id, Predecessor, Successor, Store, Next);
        {notify, New} ->
            % io:format("2~n"),
            {Pred, NStore} = notify(New, Id, Predecessor, Successor, Store),
            node(Id, Pred, Successor, NStore, Next);
        {request, Peer} ->
            % io:format("3~n"),
            request(Peer, Predecessor, Next),
            node(Id, Predecessor, Successor, Store, Next);
        {status, Pred, Nx} ->
            % io:format("4~n"),
            {Succ, Nxt} = stabilize(Pred, Nx, Id, Successor),
            node(Id, Predecessor, Succ, Store, Nxt);
        stabilize ->
            % stabilize now!!
            % io:format("S: ~w~n", [Successor]),
            % io:format("5~n"),
            stabilize(Successor),
            node(Id, Predecessor, Successor, Store, Next);

        {add, Key, Value, Qref, Client} ->
            Added = add(Key, Value, Qref, Client,
            Id, Predecessor, Successor, Store),
            node(Id, Predecessor, Successor, Added, Next);
        {lookup, Key, Qref, Client} ->
            lookup(Key, Qref, Client, Id, Predecessor, Successor, Store),
            node(Id, Predecessor, Successor, Store, Next);
        {handover, Elements} ->
            Merged = storage:merge(Store, Elements),
            node(Id, Predecessor, Successor, Merged, Next);

        {'DOWN', Ref, process, _, _} ->
            {Pred, Succ, Nxt} = down(Ref, Predecessor, Successor, Next),
            node(Id, Pred, Succ, Store, Nxt);

        {kill, Id} ->
            exit(-1);
        {kill, Other} ->
            {_, _, Spid} = Successor,
            Spid ! {kill, Other},
            node(Id, Predecessor, Successor, Store, Next);

        probe ->
            % io:format("~w: ~w~n", [Id, Successor]),
            % io:format("6~n"),
            % io:format("[~w]: Store: ~w~n", [Id, Store]),
            create_probe(Id, Successor),
            node(Id, Predecessor, Successor, Store, Next);
        {probe, Id, Nodes, T} ->
            remove_probe(T, Nodes),
            node(Id, Predecessor, Successor, Store, Next);
        {probe, Ref, Nodes, T} ->
            % io:format("[~w]: Store: ~w~n", [Id, Store]),
            forward_probe(Ref, T, Nodes, Id, Successor),
            node(Id, Predecessor, Successor, Store, Next);
        node_status ->
            io:format("[~w] Pre: ~w Suc: ~w~n", [Id, Predecessor, Successor]),
            node(Id, Predecessor, Successor, Store, Next);

        Else ->
            io:format("[~w] Node received invalid message: ~w~n", [Id, Else]),
            node(Id, Predecessor, Successor, Store, Next)
    end.


% Key value Store
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
add(Key, Value, Qref, Client, Id, {Pkey, _,  _}, {_, _, Spid}, Store) ->
    case key:between(Key, Pkey, Id) of
        true ->
            % We are responsible for this key!
            Client ! {Qref, ok},
            storage:add(Key, Value, Store);
        false ->
            % Pass it on!
            Spid ! {add, Key, Value, Qref, Client},
            Store
    end.

lookup(Key, Qref, Client, Id, {Pkey, _, _}, Successor, Store) ->
    case key:between(Key, Pkey, Id) of
        true ->
            % This is our key!
            Result = storage:lookup(Key, Store),
            Client ! {Qref, Result};
        false ->
            % Pass it on!
            {_, _, Spid} = Successor,
            Spid ! {lookup, Key, Qref, Client}
    end.

handover(Id, Store, Nkey, Npid) ->
    {Keep, Rest} = storage:split(Id, Nkey, Store),
    Npid ! {handover, Rest},
    % io:format("Handover: ~w~n", [Rest]),
    Keep.


% Process monitoring
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
monitor(Pid) ->
    erlang:monitor(process, Pid).

drop(nil) ->
    ok;
drop(Pid) ->
    erlang:demonitor(Pid, [flush]).

down(Ref, {_, Ref, _}, Successor, Next) ->
    {nil, Successor, Next};
down(Ref, Predecessor, {_, Ref, _}, {Nkey, Npid}) ->
    Nref = monitor(Npid),
    stabilize({Nkey, Nref, Npid}),
    {Predecessor, {Nkey, Nref, Npid}, nil};
down(A, B, C, D) ->
    io:format("Ring broken: ~w : ~w : ~w : ~w~n", [A, B, C, D]).

% Stabilize
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
schedule_stabilize() ->
    timer:send_interval(?StabilizeTimeout, self(), stabilize).

stabilize({_, _, Spid}) ->
    % io:format("~w~n", [Spid]),
    Spid ! {request, self()}.

% Returns new precededing node
notify({Nkey, Npid}, Id, Predecessor, Successor, Store) ->
    case Predecessor of
        nil ->
            % Always accept if we dont have a predecessor
            Keep = handover(Id, Store, Nkey, Npid),
            Nref = monitor(Npid),
            {{Nkey, Nref, Npid}, Keep};
        {Pkey, Pref, _} ->
            case key:between(Nkey, Pkey, Id) of
                true ->
                    % New key is between!
                    % We need to change monitor mode!
                    drop(Pref),
                    Nref = monitor(Npid),
                    Keep = handover(Id, Store, Nkey, Npid),
                    {{Nkey, Nref, Npid}, Keep};
                false ->
                    % request is ingored. we will tell the node
                    % io:format("Ignored request to change predecessor~n"),
                    {Pkey, _, Ppid} = Predecessor,
                    {Skey, _, Spid} = Successor,
                    Npid ! {status, {Pkey, Ppid}, {Skey, Spid}},
                    {Predecessor, Store}
            end
    end.

request(Peer, Predecessor, Next) ->
    case Predecessor of
        nil ->
            Peer ! {status, nil, Next};
        {Pkey, _Pref, Ppid} ->
            Peer ! {status, {Pkey, Ppid}, Next}
    end.

% Return our succeeding node
% Pred is what our succeeding node think is its predecessor
% Next is the Successor of our successor.
% Successor is our current successor
stabilize(Pred, Next, Id, Successor) ->
    % io:format("[~w] Stabilizing. Pr: ~w Suc: ~w~n", [Id, Pred, Successor]),
    {Skey, Sref, Spid} = Successor,
    case Pred of
        nil ->
            % The succeeding node does not have a predecessor. Inform it!
            Spid ! {notify, {Id, self()}},
            {Successor, Next};
        {Id, _} ->
            % We are already the precededing node. Done!
            {Successor, Next};
        {Skey, _} ->
            % Node has itself as predecessor. We will insert ourselves there!
            Spid ! {notify, {Id, self()}},
            {Successor, Next};
        {Xkey, Xpid} ->
            case key:between(Xkey, Id, Skey) of
                true ->
                    % The key that our succeeding node has as precessor is between us
                    % and it. We will change our succeeding node.
                    Xpid ! {notify, {Id, self()}},
                    drop(Sref),
                    Nref = monitor(Xpid),
                    {Skey, _, Spid} = Successor,
                    {{Xkey, Nref, Xpid}, {Skey, Spid}};
                false ->
                    % This node is between the succeeding node and the node it
                    % currently has as the predecessor.
                    Spid ! {notify, {Id, self()}},
                    {Successor, Next}
            end
    end.

% Probing
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% Sends a new probe to successor
create_probe(Id, {_Sid, _Sref, Spid}) ->
    Spid ! {probe, Id, [Id], erlang:system_time(micro_seconds)}.

% Probe circulated the complete circle. Print status!
remove_probe(T, Nodes) ->
    io:format("[Probe]: ~wµs ~n nodes: ~w~n", [erlang:system_time(micro_seconds)-T, lists:reverse(Nodes)]).

% Forward this probe
forward_probe(Ref, T, Nodes, Id, {_Sid, _Sref, Spid}) ->
    % io:format("[~w] Forwarding probe~n", [Id]),
    Spid ! {probe, Ref, [Id | Nodes], T}.
