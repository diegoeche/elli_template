% @license MIT License
% @doc Main HTTP request handler.
-module (myapp_http_handler).
-export ([handle/2, handle_event/3]).
-behaviour (elli_handler).


% Delegate to handler matching on HTTP verbs.
handle(Req, _Args) ->
    handle(elli_request:method(Req), elli_request:path(Req), Req).


% Template test handler.
handle('GET', _Path = [], _Req) ->
    Body = <<"Hello World!\n">>,
    {200, _Headers = [], Body};


% Catch-all handler returning `404 Not Found`.
handle(_Verb, _Path, _Req) ->
    {404, [], <<"Not Found">>}.


% Request lifecycle-event handler, where `Event` may be:
% * request_complete - After the handler's response was send to the client,
% * client_closed - In case the connection was unexpectedly closed,
% * client_timeout - In case the connection timed out,
% * request_throw - If the handler throws an exception,
% * request_error - If the handler produces and error,
% * request_exit - If the handler produces and exit,
% * request_parse_error - In case there was an error parsing a received request,
% The expected return value is `ok`.
handle_event(_Event, _Data, _Args) -> ok.
