-module(rudy_cons).
-export([init/2, start/1, stop/0]).

start(Nr) ->
    P = spawn_link(rudy_cons, init, [8080, Nr]),
    register(rudy, P).

stop() ->
    exit(whereis(rudy), "Server killed brutally").

init(Port, NrOfWorkers) ->
    Opt = [list, {active, false}, {reuseaddr, true}],
    case gen_tcp:listen(Port, Opt) of
        {ok, Listen} ->
            % Consumer handler runs is the background.
            CH = consumers:start_handler(),
            spawn_workers(CH, NrOfWorkers),

            % Listen for incoming requests
            incoming_request_handler(Listen, CH, 0),

		    % This means the server died!
            gen_tcp:close(Listen),
            ok;
        {error, Error} ->
            io:format("rudy [Init]: error: ~w~n", [Error]),
            error
    end.

% Spawns workers in separate threads.
spawn_workers(_, 0) ->
    ok;
spawn_workers(CH, I) ->
    consumers:register_consumer(CH,
        fun({Client, I}) ->
            Recv = gen_tcp:recv(Client, 0),
            case Recv of
                {ok, Str} ->
				    % Create the response and send to client.
                    Request = http:parse_request(Str),
                    Response = reply(Request),
                    gen_tcp:send(Client, Response);
                {error, Error} ->
                    io:format("rudy [RequestHandler]: error: ~w in id: ~w~n", [Error, I])
            end,
            gen_tcp:close(Client)
        end
    ),
    spawn_workers(CH, I - 1).

% This function will continiously listen for new tcp connections 
incoming_request_handler(Listen, CH, I) ->
    case gen_tcp:accept(Listen) of
        {ok, Client} ->
            % Make sure this client sends messages to us.
            % io:format("Received request~w~n", [Client]),
            consumers:add_data(CH, {Client, I}), % This adds the data to process queue
            incoming_request_handler(Listen, CH, I+1);
        {error, Error} ->
            io:format("rudy [Server]: error: ~w~n", [Error]),
            error
    end.

reply(Request) ->
	% Reading the requested file.
	{{get, FileName, _HTTPVersion}, _Args, _Message} = Request,
	if 
	FileName == [$/, $t] -> % This is a test block with extra "load"
    	http:ok("Response"),
		timer:sleeps(40);
	FileName == [$/]->
	    binay_file_request("index.html");
	true ->
	    binay_file_request([$. | FileName])
	end.

% This can read practically any file and just converts it to an
% array before sending it.
binay_file_request(FileName) ->
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
			case file:read_file("static/404.html") of
				{ok, Binary} ->
					http:fnf(binary_to_list(Binary));
				{error, Reason} ->
					io:format("~w~n", [Reason]),
					http:fnf() % Even the 404 page is missing!!
			end
	end.
