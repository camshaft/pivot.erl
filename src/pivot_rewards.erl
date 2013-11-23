%%
%% pivot_rewards.erl
%%
-module(pivot_rewards).

-export([list/1]).
-export([get/1]).
-export([set/1]).
-export([clear/1]).

-include("pivot.hrl").

-define(REWARDS_BUCKET(Env), <<"rewards:", Env/binary>>).
-define(REWARDS_KEY(App), <<App/binary>>).

list(#pivot_req{env = Env, app = App}) ->
  case riakou:do(?REWARDS_GROUP, fetch_type, [{<<"map">>, ?REWARDS_BUCKET(Env)}, ?REWARDS_KEY(App)]) of
    {ok, Obj} ->
      {ok, [{E, Reward} || {{E, _}, Reward} <- riakc_map:value(Obj)]};
    {error, {notfound, _}} ->
      {error, notfound};
    Error ->
      Error
  end.

get(#pivot_req{env = Env, app = App, event = Event}) ->
  case riakou:do(?REWARDS_GROUP, fetch_type, [{<<"map">>, ?REWARDS_BUCKET(Env)}, ?REWARDS_KEY(App)]) of
    {ok, Obj} ->
      case fast_key:get({Event, register}, riakc_map:value(Obj)) of
        undefined ->
          {error, notfound};
        Reward ->
          {ok, Reward}
      end;
    {error, {notfound, _}} ->
      {error, notfound};
    Error ->
      Error
  end.

set(#pivot_req{env = Env, app = App, event = Event, reward = Reward}) ->
  Fun = fun(Map) ->
    riakc_map:update({Event, register}, fun(Reg) ->
      riakc_register:set(Reward, Reg)
    end, Map)
  end,
  BucketAndType = {<<"map">>, ?REWARDS_BUCKET(Env)},
  Key = ?REWARDS_KEY(App),
  Options = [create],
  Args = [Fun, BucketAndType, Key, Options],
  riakou:do(?REWARDS_GROUP, modify_type, Args).

clear(#pivot_req{env = Env, app = App}) ->
  riakou:do(?REWARDS_GROUP, delete, [?REWARDS_BUCKET(Env), ?REWARDS_KEY(App)]).
