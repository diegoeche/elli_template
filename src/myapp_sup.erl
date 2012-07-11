% @license MIT License
% @doc Main OTP supervisor module for myapp.
-module (myapp_sup).
-compile (export_all).
-behaviour (supervisor).
-export ([start_link/0, init/1]).


% Creates the main supervisor process as part of myapp's supervision tree.
start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).


% Callback; Builds myapp's main supervisor spec.
init([]) ->
    % Webserver configuration.
    ElliOpts = [
        {callback, myapp_http_handler},
        {port, et_config:get(http_port)}],

    % Elli as the only child for now.
    et_sup:spec([
        et_sup:child(myapp_httpd, {elli, start_link, [ElliOpts]})
        ]).
