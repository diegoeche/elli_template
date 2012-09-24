% @license MIT License
% @doc Application Client API.
-module (myapp).
-compile (export_all).


% Starts myapp including all its dependencies.
start() ->
    start_deps(),
    application:start(myapp).


% Starts all of myapp's dependencies.
start_deps() ->
    Deps = [crypto, public_key, ssl, inets],
    [application:start(App) || App <- Deps].
