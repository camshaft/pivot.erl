-module(pivot_test_mab_arms_db).

-export([set/3]).
-export([all/2]).
-export([enabled/2]).

set(_App, _Bandit, _Arms) ->
  ok.

all(_App, _Bandit) ->
  enabled(_App, _Bandit).

enabled(_App, Bandit) ->
  {ok, [<<Bandit/binary, "-arm-1">>, <<Bandit/binary, "-arm-2">>, <<Bandit/binary, "-arm-3">>]}.
