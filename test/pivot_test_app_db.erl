-module(pivot_test_app_db).

-export([bandits/1]).
-export([add/2]).

bandits(<<"empty">>) ->
  {ok, []};
bandits(_) ->
  {ok, [<<"bandit-1">>, <<"bandit-2">>, <<"bandit-3">>]}.

add(_App, _Bandit) ->
  ok.
