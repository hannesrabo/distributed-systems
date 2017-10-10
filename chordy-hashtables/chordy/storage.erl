-module(storage).
% -export([create/0, add/3, lookup/2, split/3, merge/2]).
-compile(export_all).

create() ->
    maps:new().

add(Key, Value, Store) ->
    maps:put(Key, Value, Store).

lookup(Key, Store) ->
    case maps:find(Key, Store) of
        {ok, Value} ->
            {Key, Value};
        error ->
            false
    end.

% Takes all keys from From to To and puts them in the updated list.
% The rest are put in the Rest list
split(From, To, Store) ->
    Keep = maps:filter(
    fun(Key, _) ->
        key:between(Key, From, To)
    end, Store),
    Rest = maps:without(maps:keys(Keep), Store),
    {Keep, Rest}.


merge(Entries, Store) ->
    maps:merge(Entries, Store).
