-module (myapp_http_handler_test).
-include_lib ("etest/include/etest.hrl").
-include_lib ("etest_http/include/etest_http.hrl").
-compile (export_all).


% Test in context of the running application.
before_suite() -> myapp:start().
after_suite() -> application:stop(myapp).


% myapp_http_handler:handle/3.
test_template() ->
    % When performing a request to the handler template,
    Resp = ?perform_get("http://localhost:4000"),

    % The response should be a 200 OK.
    ?assert_status(200, Resp).
