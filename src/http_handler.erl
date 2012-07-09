-module(http_handler).
-export([handle/2, handle_event/3]).
-behaviour(elli_handler).

handle(Req, _Args) ->
  handle(elli_request:method(Req), elli_request:path(Req), Req).

handle('GET',[], _Req) ->
  {ok, [], <<"Hello World!\n">>};

handle(_, _, _) ->
  {404, [], <<"Not Found">>}.

%% @doc: Handle request events, like request completed, exception
%% thrown, client timeout, etc. Must return 'ok'.
handle_event(_Event, _Data, _Args) -> ok.
