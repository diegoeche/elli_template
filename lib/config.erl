%% @author Johannes Huning <johannes.huning@wooga.net>
%% @author Diego Echeverri <diego.echeverri@wooga.net>
%% @copyright 2012 Wooga GmbH
%% @doc Configuration access module.
-module (config).
-compile (export_all).

% Returns the applications root directory.
app_root() ->
  Cmd = "cd " ++ filename:dirname(?FILE) ++ "/..; pwd | tr -d '\n'",
  case os:getenv("APP_ROOT") of
    false -> os:cmd(Cmd);
    Value -> Value
  end.

% Loads all configuration and environment settings from the appropriate
% .config file. See config/enviroments.
init() ->
  EnvConfig = app_root() ++ "/config/environments/" ++ atom_to_list(env()) ++ ".config",
  {ok, [Terms]} = file:consult(EnvConfig),

  LoadAppConfig = fun({App, PList}) ->
                      SetEnv = fun({Key, Value}) ->
                                   application:set_env(App, Key, Value)
                               end,
                      lists:foreach(SetEnv, PList)
                  end,
  lists:foreach(LoadAppConfig, Terms).

%% @doc Alias for environment.
env() -> environment().

%% @doc Returns the current environment running in.
environment() ->
  case os:getenv("APP_ENV") of
    false -> development;
    Value -> list_to_atom(Value)
  end.

%% @doc Attempts to get the given key from the current environment,
%%      returns the Default otherwise.
get(Param, Default) ->
  ?MODULE:get(app, Param, Default).

%% @doc Attempts to get the given key from the specified app's environment,
%%      returns the Default otherwise.
get(App, Param, Default) ->
  case application:get_env(App, Param) of
    {ok, Value} -> Value;
    _ -> Default
  end.
