-module(pivot_test).

-include_lib("eunit/include/eunit.hrl").
-include("../src/pivot_clients_api.hrl").

first_test() ->
  ok = inets:start(),
  ok = riakou:start(),
  application:start(crypto),
  ok = riakou:start_link(simple_env:get_binary("RIAK_URL", <<"riak://localhost">>)),
  ok = riakou:wait_for_connection(),

  PReq = #pivot_req{
    env = <<"test">>,
    app = <<"app">>
  },

  test_rewards(PReq),
  test_bandits(PReq),
  test_arms(PReq).

test_rewards(PReq) ->
  ok = pivot_client:do(rewards, clear, PReq),

  ok = pivot_client:do(rewards, set, PReq#pivot_req{
    reward = <<"0.1">>,
    event = <<"event1">>
  }),

  ok = pivot_client:do(rewards, set, PReq#pivot_req{
    reward = <<"0.9">>,
    event = <<"event2">>
  }),

  {ok, <<"0.9">>} = pivot_client:do(rewards, get, PReq#pivot_req{
    event = <<"event2">>
  }),

  {ok, Pairs} = pivot_client:do(rewards, list, PReq#pivot_req{}),
  assert_key(<<"event1">>, <<"0.1">>, Pairs),
  assert_key(<<"event2">>, <<"0.9">>, Pairs).

test_bandits(PReq) ->
  ok = pivot_client:do(bandits, clear, PReq),

  ok = pivot_client:do(bandits, enable, PReq#pivot_req{
    bandit = <<"bandit1">>
  }),
  ok = pivot_client:do(bandits, enable, PReq#pivot_req{
    bandit = <<"bandit2">>
  }),

  {ok, EnabledBandits} = pivot_client:do(bandits, enabled, PReq#pivot_req{}),
  assert_member(<<"bandit1">>, EnabledBandits),
  assert_member(<<"bandit2">>, EnabledBandits),

  ok = pivot_client:do(bandits, disable, PReq#pivot_req{
    bandit = <<"bandit2">>
  }),

  {ok, EnabledBandits2} = pivot_client:do(bandits, enabled, PReq#pivot_req{}),
  assert_member(<<"bandit1">>, EnabledBandits2),
  assert_not_member(<<"bandit2">>, EnabledBandits2),

  {ok, DisabledBandits} = pivot_client:do(bandits, disabled, PReq#pivot_req{}),
  assert_not_member(<<"bandit1">>, DisabledBandits),
  assert_member(<<"bandit2">>, DisabledBandits),

  ok = pivot_client:do(bandits, enable, PReq#pivot_req{
    bandit = <<"bandit3">>
  }),
  ok = pivot_client:do(bandits, enable, PReq#pivot_req{
    bandit = <<"bandit4">>
  }),

  ok = pivot_client:do(bandits, remove, PReq#pivot_req{
    bandit = <<"bandit3">>
  }),

  {ok, Bandits} = pivot_client:do(bandits, list, PReq#pivot_req{}),
  assert_key(<<"bandit1">>, true, Bandits),
  assert_key(<<"bandit2">>, false, Bandits),
  assert_key(<<"bandit3">>, undefined, Bandits),
  assert_key(<<"bandit4">>, true, Bandits).

test_arms(PReq) ->
  ok = pivot_client:do(arms, enable, PReq#pivot_req{
    bandit = <<"bandit1">>,
    arm = <<"arm1">>
  }),
  ok = pivot_client:do(arms, enable, PReq#pivot_req{
    bandit = <<"bandit2">>,
    arm = <<"arm1">>
  }).

assert_key(Key, Value, List) ->
  ?assertEqual(fast_key:get(Key, List), Value).

assert_member(Key, List) ->
  ?assert(lists:member(Key, List)).
assert_not_member(Key, List) ->
  ?assertNot(lists:member(Key, List)).
