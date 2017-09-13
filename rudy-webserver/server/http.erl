-module(http).
-export([parse_request/1, ok/1, ok/2, fnf/1, fnf/0, get/1]).

parse_request(R0) ->
    {RequestLine, R1}   = request_line(R0),
    {Headers, R2}       = headers(R1),
    {Body, _}           = body(R2),
    {RequestLine, Headers, Body}.

% Decodes the request line. [Method SP Request-URI SP HTTP-Version CRLF]
request_line([$G, $E, $T, 32 | Rl0]) ->
    {URI, Rl1} = request_uri(Rl0),  % Get the uri
    {Ver, Rl2} = http_version(Rl1), % Get version
    [13, 10 | Rl3] = Rl2,           % Get the rest of the message (Removes the clear line from the rest of the message).
    {{get, URI, Ver}, Rl3}.

% Iterate through the input until we hit a space (32) Then return the output
request_uri([32 | Rest]) ->
    {[], Rest};
request_uri([Char | Rest]) ->
    {UriRest, MsgRest} = request_uri(Rest),
    {[Char | UriRest], MsgRest}.

http_version([$H, $T, $T, $P, $/, $1, $., $0 | Rest]) ->
    {v10, Rest};
http_version([$H, $T, $T, $P, $/, $1, $., $1 | Rest]) ->
    {v11, Rest}.

% Iterate through characters in headers until it reaches linefeed after headers (13, 10)
% Each header is also ended with a linefeed.
headers([13, 10 | Rest]) ->
    {[], Rest};
headers(Headers) ->
    {Header, Rest} = header(Headers),   %Extract one header from the list of chars
    {HRest, ReqRest} = headers(Rest),
    {[Header | HRest], ReqRest}.

% Extract one header. Stop when a linefeed is found (13, 10)
header([13, 10 | Rest]) ->
    {[], Rest};
header([Char | Rest]) ->
    {CurrHeaderRest, MsgRest} = header(Rest),
    {[Char | CurrHeaderRest], MsgRest}.

body(ReqMsg) ->
    {ReqMsg, []}.

ok(Body) ->
    "HTTP/1.1 200 OK\r\n" ++ "\r\n" ++ Body.

ok(Body, ContentType) ->
    "HTTP/1.1 200 OK\r\n" ++ ContentType ++ "\r\n\r\n" ++ Body.

fnf() ->
    "HTTP/1.1 404 Page not found\r\n" ++ "\r\n\r\n <h1>404 - File not found</h1>".

fnf(Body) ->
    "HTTP/1.1 404 Page not found\r\n" ++ "\r\n" ++ Body.

get(URI) ->
    "GET " ++ URI ++ " HTTP/1.1\r\n" ++ "\r\n".
