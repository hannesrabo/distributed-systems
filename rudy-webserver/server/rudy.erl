-module(rudy).
-export([init/1, start/0, stop/0, request/1]).

start() ->
    P = spawn_link(rudy, init, [8080]),
    register(rudy, P).

stop() ->
    exit(whereis(rudy), "Server killed brutally").

init(Port) ->
    Opt = [list, {active, false}, {reuseaddr, true}],
    case gen_tcp:listen(Port, Opt) of
        {ok, Listen} ->
            handler(Listen),
            gen_tcp:close(Listen),
            ok;
        {error, Error} ->
            io:format("rudy [Init]: error: ~w~n", [Error]),
            error
    end.

handler(Listen) ->
    case gen_tcp:accept(Listen) of
        {ok, Client} ->
            spawn_link(rudy, request, [Client]), % spawn one process per request -- maybe too much?
            handler(Listen);
        {error, Error} ->
            io:format("rudy [Server]: error: ~w~n", [Error]),
            error
    end.

request(Client) ->
    Recv = gen_tcp:recv(Client, 0),
    case Recv of
        {ok, Str} ->
            Request = http:parse_request(Str),
            Response = reply(Request),
            gen_tcp:send(Client, Response);
        {error, Error} ->
            io:format("rudy [RequestHandler]: error: ~w~n", [Error])
    end,
    gen_tcp:close(Client).

reply({{get, URI, _}, _, _}) ->
    timer:sleep(40),
    http:ok("This is the website: " ++ URI).
