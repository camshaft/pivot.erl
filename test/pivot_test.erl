-module(pivot_test).

-include_lib("eunit/include/eunit.hrl").

full_test() ->
  Ref = my_test,
  AppID = <<"app">>,
  UserID = <<"user">>,

  %% Ref, UserDB, MabStateDB, MabConfigDB, EventDB, AppDB
  pivot:start(Ref, pivot_test_user_db, pivot_test_mab_arms_db, pivot_test_mab_state_db, pivot_test_mab_config_db, pivot_test_event_db, pivot_test_app_db),

  ok = pivot:register(Ref, AppID, <<"bandit-1">>, [
    {<<"arm-1">>, true},
    {<<"arm-2">>, false},
    {<<"arm-3">>, true}
  ], [
    {algorithm, pivot_mab_ucb1}
  ]),

  ok = pivot:track(Ref, AppID, <<"medium-event">>, UserID),

  {ok, _Assignments} = pivot:assign(Ref, AppID, UserID),

  {ok, 250, _} = pivot:report(Ref, AppID, <<"bandit-1">>),
  {ok, _} = pivot:list(Ref, AppID),

  ok = pivot:configure(Ref, AppID, <<"bandit-1">>, []),

  {ok, Events} = pivot:get_events(Ref, AppID),

  [pivot:get_event(Ref, AppID, Event) || {Event, _Reward} <- Events],

  ok = pivot:set_event(Ref, AppID, <<"new-event">>, 0.8),

  pivot:stop(Ref).
