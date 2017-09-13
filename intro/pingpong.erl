-module(pingpong).
-export([pong/0, ping/0]).

pong() ->
    register(ponger, spawn(
        fun PongFunction() ->
            receive
                {ping, PID} ->
                    io:format("Received pong~n"),
                    timer:sleep(500),
                    PID ! {pong, self()}
            end,

            PongFunction()
        end
    )).

ping() ->
    register(pinger, spawn(
        fun() ->
            {ponger, 'one@hannes-r'} ! {ping, self()},

            ping_function()
        end
    )).

ping_function() ->
    receive
        {pong, PID} ->
            io:format("Received ping~n"),
            timer:sleep(500),
            PID ! {ping, self()}
    end,
    ping_function().
