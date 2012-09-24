% @author Johannes Huning <johannes.huning@wooga.net>
% @author Diego Echeverri <diego.echeverri@wooga.net>
-module (myapp_config).
-compile (export_all).


% Returns the application's root directory.
app_root() ->
    Cmd = "cd " ++ filename:dirname(?FILE) ++ "/..; pwd | tr -d '\n'",
    case os:getenv("APP_ROOT") of
        false -> os:cmd(Cmd);
        Value ->Value
    end.


% Loads all configuration and environment settings from the appropriate
% .config file. See `config/enviroments`.
init() ->
    EnvConfig = app_root() ++ "/config/environments/" ++
        atom_to_list(env()) ++ ".config",

    Proplist = case file:consult(EnvConfig) of
        {ok, [Terms]} -> Terms;
        _             -> []
    end,

    LoadAppConfig = fun({App, PList}) ->
        SetEnv = fun({Key, Value}) ->
            application:set_env(App, Key, Value)
        end,
        lists:foreach(SetEnv, PList)
    end,

    lists:foreach(LoadAppConfig, Proplist).


% Alias for environment/0.
env() -> environment().

% Returns the current environment running in.
environment() ->
    case os:getenv("ERL_ENV") of
        false -> development;
        Value -> list_to_atom(Value)
    end.
