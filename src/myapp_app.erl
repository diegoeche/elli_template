% @license MIT License
% @doc Main OTP application callback module.
-module (myapp_app).
-behaviour (application).
-export ([start/2, stop/1]).


% Called upon starting myapp; Starts myapp's main supervisor.
start(_StartType, _StartArgs) ->
    % Load the enviroment configuration from `config/enviroments` first.
    myapp_config:init(),
    myapp_sup:start_link().


% Callback; Returns just `ok`.
% State is taken from the return value of start/2.
stop(_State) -> ok.
