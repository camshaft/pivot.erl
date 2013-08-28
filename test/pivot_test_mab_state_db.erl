-module(pivot_test_mab_state_db).

-export([init/3]).
-export([update/3]).
-export([get/3]).

init(_App, _Bandit, _State) ->
  ok.

update(_App, _Score, _BanditArmPairs) ->
  ok.

get(_App, _Bandit, Arms) ->
  {ok, [state(Arm) || Arm <- Arms]}.

state(<<"bandit-1-arm-1">> = Arm) ->
  {Arm, {100, 80.0}};
state(<<"bandit-1-arm-2">> = Arm) ->
  {Arm, {90, 60.0}};
state(Arm) ->
  {Arm, {60, 5.0}}.
