-module(node2).
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
    {ok, Successor} = connect(Id, Peer),
    io:format("Successfully connected~n"),
    schedule_stabilize(),
    node(Id, Predecessor, Successor, storage:create()).

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
node(Id, Predecessor, Successor, Store) ->
    % io:format("[~w] P: ~w, S: ~w~n", [Id, Predecessor, Successor]),
    receive
        {key, Qref, Peer} ->
            % io:format("1~n"),
            Peer ! {Qref, Id},
            node(Id, Predecessor, Successor, Store);
        {notify, New} ->
            % io:format("2~n"),
            {Pred, NStore} = notify(New, Id, Predecessor, Store),
            node(Id, Pred, Successor, NStore);
        {request, Peer} ->
            % io:format("3~n"),
            request(Peer, Predecessor),
            node(Id, Predecessor, Successor, Store);
        {status, Pred} ->
            % io:format("4~n"),
            Succ = stabilize(Pred, Id, Successor),
            node(Id, Predecessor, Succ, Store);
        stabilize ->
            % stabilize now!!
            % io:format("S: ~w~n", [Successor]),
            % io:format("5~n"),
            stabilize(Successor),
            node(Id, Predecessor, Successor, Store);

        {add, Key, Value, Qref, Client} ->
            Added = add(Key, Value, Qref, Client,
            Id, Predecessor, Successor, Store),
            node(Id, Predecessor, Successor, Added);
        {lookup, Key, Qref, Client} ->
            lookup(Key, Qref, Client, Id, Predecessor, Successor, Store),
            node(Id, Predecessor, Successor, Store);
        {handover, Elements} ->
            Merged = storage:merge(Store, Elements),
            node(Id, Predecessor, Successor, Merged);

        probe ->
            % io:format("~w: ~w~n", [Id, Successor]),
            % io:format("6~n"),
            io:format("[~w]: Store: ~w~n", [Id, Store]),
            create_probe(Id, Successor),
            node(Id, Predecessor, Successor, Store);
        {probe, Id, Nodes, T} ->
            remove_probe(T, Nodes),
            node(Id, Predecessor, Successor, Store);
        {probe, Ref, Nodes, T} ->
            io:format("[~w]: Store: ~w~n", [Id, Store]),
            forward_probe(Ref, T, Nodes, Id, Successor),
            node(Id, Predecessor, Successor, Store);
        node_status ->
            io:format("[~w] Pre: ~w Suc: ~w~n", [Id, Predecessor, Successor]),
            node(Id, Predecessor, Successor, Store);

        Else ->
            io:format("[~w] Node received invalid message: ~w~n", [Id, Else]),
            node(Id, Predecessor, Successor, Store)
    end.


% Key value Store
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
add(Key, Value, Qref, Client, Id, {Pkey, _}, {_, Spid}, Store) ->
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

lookup(Key, Qref, Client, Id, {Pkey, _}, Successor, Store) ->
    case key:between(Key, Pkey, Id) of
        true ->
            % This is our key!
            Result = storage:lookup(Key, Store),
            Client ! {Qref, Result};
        false ->
            % Pass it on!
            {_, Spid} = Successor,
            Spid ! {lookup, Key, Qref, Client}
    end.

handover(Id, Store, Nkey, Npid) ->
    {Keep, Rest} = storage:split(Id, Nkey, Store),
    Npid ! {handover, Rest},
    io:format("Handover: ~w~n", [Rest]),
    Keep.


% Stabilize
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
schedule_stabilize() ->
    timer:send_interval(?StabilizeTimeout, self(), stabilize).

stabilize({_, Spid}) ->
    % io:format("~w~n", [Spid]),
    Spid ! {request, self()}.

% Returns new precededing node
notify({Nkey, Npid}, Id, Predecessor, Store) ->
    case Predecessor of
        nil ->
            % Always accept if we dont have a predecessor
            Keep = handover(Id, Store, Nkey, Npid),
            {{Nkey, Npid}, Keep};
        {Pkey, _} ->
            case key:between(Nkey, Pkey, Id) of
                true ->
                    % New key is between!
                    Keep = handover(Id, Store, Nkey, Npid),
                    {{Nkey, Npid}, Keep};
                false ->
                    % request is ingored. we will tell the node
                    io:format("Ignored request to change predecessor~n"),
                    Npid ! {status, Predecessor},
                    {Predecessor, Store}
            end
    end.

request(Peer, Predecessor) ->
    case Predecessor of
        nil ->
            Peer ! {status, nil};
        {Pkey, Ppid} ->
            Peer ! {status, {Pkey, Ppid}}
    end.

% Return our succeeding node
stabilize(Pred, Id, Successor) ->
    % io:format("[~w] Stabilizing. Pr: ~w Suc: ~w~n", [Id, Pred, Successor]),
    {Skey, Spid} = Successor,
    case Pred of
        nil ->
            % The succeeding node does not have a predecessor. Inform it!
            Spid ! {notify, {Id, self()}},
            Successor;
        {Id, _} ->
            % We are already the precededing node. Done!
            Successor;
        {Skey, _} ->
            % Node has itself as predecessor. We will insert ourselves there!
            Spid ! {notify, {Id, self()}},
            Successor;
        {Xkey, Xpid} ->
            case key:between(Xkey, Id, Skey) of
                true ->
                    % The key that our succeeding node has as precessor is between us
                    % and it. We will change our succeeding node.
                    {Xkey, Xpid};
                false ->
                    % This node is between the succeeding node and the node it
                    % currently has as the predecessor. Insert ourselves there!
                    Spid ! {notify, {Id, self()}},
                    Successor
            end
    end.

% Probing
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% Sends a new probe to successor
create_probe(Id, {_Sid, Spid}) ->
    Spid ! {probe, Id, [Id], erlang:system_time(micro_seconds)}.

% Probe circulated the complete circle. Print status!
remove_probe(T, Nodes) ->
    io:format("[Probe]: ~wµs ~n nodes: ~w~n", [erlang:system_time(micro_seconds)-T, lists:reverse(Nodes)]).

% Forward this probe
forward_probe(Ref, T, Nodes, Id, {_Sid, Spid}) ->
    io:format("[~w] Forwarding probe~n", [Id]),
    Spid ! {probe, Ref, [Id | Nodes], T}.
