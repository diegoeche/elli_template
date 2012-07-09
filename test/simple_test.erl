-module(simple_test).
-compile(export_all).

-include_lib("etest/include/etest.hrl").
-include_lib("etest_http/include/etest_http.hrl").

before_suite() ->
  application:start(app).

after_suite() ->
  application:stop(app).

test_the_basic_stuff_works() ->
  ?assert_equal(1, 1).

test_hello_world_api() ->
  Response = ?perform_get("http://localhost:3000"),
  ?assert_status(200, Response).
