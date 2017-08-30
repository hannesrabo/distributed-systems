-module(helloworld).
-export([world/0, wait/0]).

world() ->
    "Hello World".


wait() ->
    receive
        X -> io:format("Ahh received a message~s~n", [X])
    end.
