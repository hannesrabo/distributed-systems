-module(rudyc).
-export([init/2, start/1, start_multi/2, stop/0]).

start(Nr) ->
    P = spawn_link(?MODULE, init, [8080, Nr]),
    register(rudy, P).

start_multi(0, _) ->
    ok;
start_multi(Nr, SubThreads) ->
    spawn_link(rudyc, init, [8080, SubThreads]),
    start_multi(Nr - 1, SubThreads).

stop() ->
    exit(whereis(rudy), "Server killed brutally").

init(Port, NrOfWorkers) ->
    Opt = [list, {active, false}, {reuseaddr, true}],
    case gen_tcp:listen(Port, Opt) of
        {ok, Listen} ->
            % The producer and consumer handler runs is
            % the background.
            CH = consumers:start_handler(),
            spawn_workers(CH, NrOfWorkers),

            % Listen for incoming requests
            incoming_request_handler(Listen, CH, 0),
            gen_tcp:close(Listen),
            ok;
        {error, Error} ->
            io:format("rudy [Init]: error: ~w~n", [Error]),
            error
    end.

spawn_workers(_, 0) ->
    ok;
spawn_workers(CH, I) ->
    consumers:register_consumer(CH,
        fun({Client, Data}) ->
            Request = http:parse_request(Data),
            Response = reply(Request),
            gen_tcp:send(Client, Response),
            gen_tcp:close(Client)
        end
    ),
    spawn_workers(CH, I - 1).

% A client tries to connect
incoming_request_handler(Listen, CH, I) ->
    case gen_tcp:accept(Listen) of
        {ok, Client} ->
            % Make sure this client sends messages to us.
            case receive_data_loop(Client) of
                {tcp, Client, Data} ->
                    consumers:add_data(CH, {Client, Data});
                Else ->
                    io:format("Disconnected: ~w~n", [Else])
            end,


            incoming_request_handler(Listen, CH, I+1);
        {error, Error} ->
            io:format("rudy [Server]: error: ~w~n", [Error]),
            error
    end.


% receive (not looping for now)
receive_data_loop(Client) ->
    inet:setopts(Client, [{active, once}]),
    receive
        % Receive tcp data.
        {tcp, _Client, _Data} = TcpMsg->
            TcpMsg;
        % Receive client disconnected.
        {tcp_closed, Client} ->
            closed
    end.

reply({{get, URI, _}, _, _}) ->
    timer:sleep(40),
    http:ok("This is the website: " ++ URI).
