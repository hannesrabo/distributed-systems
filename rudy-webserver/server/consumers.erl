-module(consumers).
-export([start_handler/0, register_consumer/2, add_data/2, test/0]).

test() ->
    H = start_handler(),
    register_consumer(H, fun(Data) -> io:format("This is the data: ~w~n", [Data]), timer:sleep(500) end),
    timer:sleep(1000),
    add_data(H, "This is a data item"),
    add_data(H, "This is a data item"),
    add_data(H, "This is a data item"),
    add_data(H, "This is a data item"),add_data(H, "This is a data item").

start_handler() ->
    spawn_link(
        fun() ->
            consumer_handler([], [])
        end
    ).

% Distribute data with a round robbing algorithm

% Initial item
consumer_handler([], []) ->
    receive
        {register, NewConsumer} ->
            consumer_handler(
                [],
                % Add the pid of the consumer to the list
                [NewConsumer]
            )
    end;

% Reset the consumer list
consumer_handler([], InitialConsumersList) ->
    % io:format("Renew list~n"),
    consumer_handler(InitialConsumersList, InitialConsumersList);

% Regular handler
consumer_handler([ConsumerItem | ConsumersListRest] = CurrentConsumerList, InitialConsumersList) ->
    receive
        {data, _} = DataItem ->
            % io:format("Sending to:~w~n", [ConsumerItem]),
            ConsumerItem ! DataItem,
            consumer_handler(ConsumersListRest, InitialConsumersList);

        {register, NewConsumer} ->
            consumer_handler(
                CurrentConsumerList,
                % Add the pid of the new consumer to the list
                [NewConsumer | InitialConsumersList]
            )
    end.

% If this dies, we probably want to spawn a new one!
register_consumer(ConsumerHandler, ConsumerFunction) ->
    ConsumerHandler ! {
            register ,
            spawn_link(
                fun ConsumerThread() ->
                    receive
                        {data, Data} ->
                            ConsumerFunction(Data),
                            ConsumerThread()
                    end
                end
            )
        }.

add_data(ConsumerHandler, Data) ->
    ConsumerHandler ! {data, Data}.
