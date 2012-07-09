-module(root_supervisor).
-behaviour(supervisor).

%% API
-export([start_link/0]).

%% Supervisor callbacks
-export([init/1]).

%% ===================================================================
%% API functions
%% ===================================================================

start_link() -> supervisor:start_link({local, ?MODULE}, ?MODULE, []).

%% ===================================================================
%% Supervisor callbacks
%% ===================================================================

init([]) ->
  RestartStrategy = one_for_one,
  MaxRestarts = 3,
  RestartTimeframe = 10,
  Port = et_config:app_get(app, port),
  EchoElliOpts = [{callback, http_handler},
                  {port,     Port}
                 ],
  ChildSpecs = [child_spec_ (fb_update_handler, elli, [EchoElliOpts])],
  {ok, {{RestartStrategy, MaxRestarts, RestartTimeframe}, ChildSpecs}}.

%% ===================================================================
%% Private
%% ===================================================================

child_spec_(Id, Module, InitArgs) ->
  StartFunc = {Module, start_link, InitArgs},
  RestartType = permanent,
  ShutdownTimeout = 5000,
  ChildType = worker,
  Modules = [Module],
  {Id, StartFunc, RestartType, ShutdownTimeout, ChildType, Modules}.
