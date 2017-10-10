-module(key).
-export([generate/0, between/3]).

% Generate a 30 bit number. (0 - 1 000 000 000)
generate() ->
    rand:uniform(1000000000).

% Check if key is between node From and To.
% (From and To can be the same values)
between(_, From, From) ->
    true;
between(Key, From, To) ->
       ((Key > From) and (Key =< To))
    or ((Key > From) and (To < From))
    or ((Key =< To) and (From > To)).
