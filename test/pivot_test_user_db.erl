-module(pivot_test_user_db).

-export([assignments/3]).
-export([increment_usage/5]).
-export([set/4]).

assignments(_, _, _) ->
  {ok, [{<<"bandit-1">>, <<"bandit-1-arm-1">>, []}]}.

increment_usage(_Env, _App, _User, _Bandit, _Arm) ->
  ok.

set(_, _App, _User, _Bandits) ->
  ok.
