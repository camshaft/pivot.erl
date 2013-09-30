-module(pivot_test_mab_state_db).

-export([init/3]).
-export([add_reward/5]).
-export([get/3]).

init(_App, _Bandit, _State) ->
  ok.

add_reward(_Env, _App, _Score, _Bandit, _Arm) ->
  ok.

get(_App, _Bandit, Arms) ->
  {ok, [state(Arm) || Arm <- Arms]}.

state(<<"bandit-1-arm-1">> = Arm) ->
  {Arm, {100, 80.0}};
state(<<"bandit-1-arm-2">> = Arm) ->
  {Arm, {90, 60.0}};
state(Arm) ->
  {Arm, {60, 5.0}}.
