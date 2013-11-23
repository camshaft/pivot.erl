-record(pivot_req, {
  id,
  env,
  app,
  version = <<"*">>,
  token,
  event,
  bandit,
  arm,
  bandit_arm,
  arms,
  reward,
  selections,
  explore = true,
  event_set,
  count,
  score
}).

-define(SUPER_BANDIT, <<"$$SUPER_BANDIT$$">>).

-define(KEYD, <<0>>).
-define(KEY_HASH, sha).
-define(KEY_HASH(BinToHash), (crypto:hash(?KEY_HASH, BinToHash))).
-define(KEY_HASH(A, B), ?KEY_HASH([A, ?KEYD, B])).
-define(KEY_HASH(A, B, C), ?KEY_HASH([A, ?KEYD, B, ?KEYD, C])).
-define(KEY_HASH(A, B, C, D), ?KEY_HASH([A, ?KEYD, B, ?KEYD, C, ?KEYD, D])).
-define(KEY_HASH(A, B, C, D, E), ?KEY_HASH([A, ?KEYD, B, ?KEYD, C, ?KEYD, D, ?KEYD, E])).
-define(KEY_HASH(A, B, C, D, E, F), ?KEY_HASH([A, ?KEYD, B, ?KEYD, C, ?KEYD, D, ?KEYD, E, ?KEYD, F])).

-define(BANDIT_ARM_HASH(Bandit, Arm), (<<(erlang:phash2([Bandit, 0, Arm])):32>>)).
-define(BANDIT_ARM_HASH_LENGTH, 4).

%% riakou groups
-define(PIVOT_GROUP, pivot).
-define(ARM_STATE_GROUP, (?PIVOT_GROUP)).
-define(ARMS_GROUP, (?PIVOT_GROUP)).
-define(BANDITS_GROUP, (?PIVOT_GROUP)).
-define(EVENT_SET_GROUP, (?PIVOT_GROUP)).
-define(REWARDS_GROUP, (?PIVOT_GROUP)).
-define(SELECTIONS_GROUP, (?PIVOT_GROUP)).
