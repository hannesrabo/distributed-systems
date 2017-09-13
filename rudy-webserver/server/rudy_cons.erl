-module(rudy_cons).
-export([init/2, start/1, start_multi/2, stop/0]).

start(Nr) ->
    P = spawn_link(rudy_cons, init, [8080, Nr]),
    register(rudy, P).

start_multi(0, _) ->
    ok;
start_multi(Nr, SubThreads) ->
    spawn_link(rudy_cons, init, [8080, SubThreads]),
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
        fun({Client, I}) ->
            Recv = gen_tcp:recv(Client, 0),
            case Recv of
                {ok, Str} ->
                    Request = http:parse_request(Str),
                    Response = reply(Request),
                    % io:format(Response),
                    gen_tcp:send(Client, Response);
                {error, Error} ->
                    io:format("rudy [RequestHandler]: error: ~w in id: ~w~n", [Error, I])
            end,
            gen_tcp:close(Client)
        end
    ),
    spawn_workers(CH, I - 1).

% A client tries to connect
incoming_request_handler(Listen, CH, I) ->
    case gen_tcp:accept(Listen) of
        {ok, Client} ->
            % Make sure this client sends messages to us.
            % io:format("Received request~w~n", [Client]),
            consumers:add_data(CH, {Client, I}),
            incoming_request_handler(Listen, CH, I+1);
        {error, Error} ->
            io:format("rudy [Server]: error: ~w~n", [Error]),
            error
    end.

reply(Request) ->
	% We should load a page from memory here...
	% Reading the requested file.
    %timer:sleeps(40),
	{{get, FileName, _HTTPVersion}, _Args, _Message} = Request,
	if
        FileName == [$/, $t] ->
            http:ok("Response");
		FileName == [$/]->
			binay_file_request("index.html");
		true ->
			binay_file_request([$. | FileName])
	end.

% This can read practically any file and just converts it to an
% array before sending it.
binay_file_request(FileName) ->
	%io:format("GET ~p~n", [FileName]),
	case file:read_file("static/" ++ FileName) of
		{ok, Binary} ->
			%Providing the page
			%io:format("200 OK~n"),
            case filename:extension(FileName) of
                ".png" -> http:ok(binary_to_list(Binary), "Content-Type: image/png");
                ".css" -> http:ok(binary_to_list(Binary), "Content-Type: text/css");
                ".jpeg" -> http:ok(binary_to_list(Binary), "Content-Type: image/jpeg");
                ".pdf" -> http:ok(binary_to_list(Binary), "Content-Type: application/pdf");
                _ -> http:ok(binary_to_list(Binary))
            end;

		{error, _Reason} ->

		    % Trying to provide 404 page
			case file:read_file("404.html") of
				{ok, Binary} ->
					%io:format("404 Page not available~n"),
					http:fnf(binary_to_list(Binary));
				{error, _Reason} ->
					%io:format("404 Page not available (no page)~n"),
					http:fnf()
			end
	end.
