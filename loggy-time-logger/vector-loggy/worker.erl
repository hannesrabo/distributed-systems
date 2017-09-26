-module(worker).
-export([start/5, stop/1, peers/2]).

start(Name, Logger, Seed, Sleep, Jitter) ->
	spawn_link(fun() -> init(Name, Logger, Seed, Sleep, Jitter) end ).

stop(Worker) ->
	Worker ! stop.

init(Name, Logger, Seed, Sleep, Jitter) ->
	rand:seed(exsplus, {Seed, Seed, Seed}),
	receive 
		{peers, Peers} ->
			loop(Name, Logger, time:zero(), Peers, Sleep, Jitter);
		stop ->
			ok
	end.

peers(Wrk, Peers) ->
	Wrk ! {peers, Peers}.

loop(Name, Log, Time, Peers, Sleep, Jitter) ->
	Wait = rand:uniform(Sleep),
	receive
		{msg, TimeRec, Msg} ->
			NewTime = time:inc(Name, time:merge(Time, TimeRec)),
			Log ! {log, Name, NewTime, {received, Msg}},
			loop(Name, Log, NewTime, Peers, Sleep, Jitter);
		stop ->
			ok;
		Error ->
			Log ! {log, Name, Time, {error, Error}}
	after Wait ->
		Selected = select(Peers),
		NewTime = time:inc(Name, Time),
		Message = {hello, rand:uniform(100)},
		Selected ! {msg, NewTime, Message},
		jitter(Jitter),
		Log ! {log, Name, NewTime, {sending, Message}},
		loop(Name, Log, NewTime, Peers, Sleep, Jitter)
	end.

select(Peers) ->
	lists:nth(rand:uniform(length(Peers)), Peers).

jitter(0) -> ok;
jitter(Jitter) -> timer:sleep(rand:uniform(Jitter)).






