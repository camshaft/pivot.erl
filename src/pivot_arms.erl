%%
%% pivot_arms.erl
%%
-module(pivot_arms).

-export([list/1]).
-export([enabled/1]).
-export([enable/1]).
-export([disabled/1]).
-export([disable/1]).
-export([remove/1]).
-export([clear/1]).

-export([get_map/1]).
-export([update_dt/2]).

-include("pivot.hrl").

-define(ARMS_BUCKET(Env), <<"arms:", Env/binary>>).
-define(ARMS_KEY(App, Bandit), <<App/binary, ":", Bandit/binary>>).

list(Req) ->
  case get_map(Req) of
    {ok, Bandits} ->
      {ok, [{Bandit, Enabled} || {{Bandit, flag}, Enabled} <- Bandits]};
    Error ->
      Error
  end.

enabled(Req) ->
  case get_map(Req) of
    {ok, Bandits} ->
      {ok, [Bandit || {{Bandit, flag}, true} <- Bandits]};
    Error ->
      Error
  end.

enable(Req) ->
  update(Req, enable).

disabled(Req) ->
  case get_map(Req) of
    {ok, Bandits} ->
      {ok, [Bandit || {{Bandit, flag}, false} <- Bandits]};
    Error ->
      Error
  end.

disable(Req) ->
  update(Req, disable).

remove(Req = #pivot_req{bandit = Bandit}) ->
  Fun = fun(Map) ->
    riakc_map:erase({Bandit, flag}, Map)
  end,
  update_dt(Fun, Req).

clear(Req = #pivot_req{env = Env, app = App, bandit = Bandit}) ->
  case riakou:do(?ARMS_GROUP, delete, [?ARMS_BUCKET(Env), ?ARMS_KEY(App, Bandit)]) of
    ok ->
      pivot:do_async(selections, renew, Req);
    Error ->
      Error
  end.

update(Req = #pivot_req{arm = Arm}, Op) ->
  Fun = fun(Map) ->
    riakc_map:update({Arm, flag}, fun(Flag) ->
      riakc_flag:Op(Flag)
    end, Map)
  end,
  update_dt(Fun, Req).

get_map(#pivot_req{env = Env, app = App, bandit = Bandit}) ->
  case riakou:do(?ARMS_GROUP, fetch_type, [{<<"map">>, ?ARMS_BUCKET(Env)}, ?ARMS_KEY(App, Bandit)]) of
    {ok, Obj} ->
      {ok, riakc_map:value(Obj)};
    {error, {notfound, _}} ->
      {error, notfound};
    Error ->
      Error
  end.

update_dt(Fun, Req = #pivot_req{env = Env, app = App, bandit = Bandit}) ->
  BucketAndType = {<<"map">>, ?ARMS_BUCKET(Env)},
  Key = ?ARMS_KEY(App, Bandit),
  Options = [create],
  Args = [Fun, BucketAndType, Key, Options],

  case riakou:do(?ARMS_GROUP, modify_type, Args) of
    ok ->
      pivot:do_async(selections, renew, Req);
    Error ->
      Error
  end.
