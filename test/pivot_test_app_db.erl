-module(pivot_test_app_db).

-export([bandits/1]).
-export([add/3]).

bandits(<<"empty">>) ->
  {ok, []};
bandits(_) ->
  {ok, [<<"bandit-1">>, <<"bandit-2">>, <<"bandit-3">>]}.

add(_Env, _App, _Bandit) ->
  ok.
