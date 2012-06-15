-module(app).
-behaviour(application).

%% Application callbacks
-export([start/2, start_with_dependencies/0, stop/1]).

%% ===================================================================
%% Application callbacks
%% ===================================================================

start(_StartType, _StartArgs) ->
  config:init(),
  root_supervisor:start_link().

%% @doc Starts the backend dependencies.
start_dependencies() ->
  Apps = [sasl, crypto, public_key, ssl, inets],
  [application:start(App) || App <- Apps].

%% @doc Starts the backend including all its dependencies. Used with CT.
start_with_dependencies() ->
  start_dependencies(),
  application:start(app).

stop(_State) ->
  ok.
