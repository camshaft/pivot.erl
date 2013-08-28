-module(pivot_test_user_db).

-export([assignments/2]).
-export([set/3]).

assignments(_, _) ->
  {ok, [{<<"bandit-1">>, <<"bandit-1-arm-1">>, []}]}.

set(_App, _User, _Bandits) ->
  ok.
