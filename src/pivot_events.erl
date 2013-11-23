%%
%% pivot_events.erl
%%
-module(pivot_events).

-export([add/1]).

-include("pivot.hrl").

add(Req) ->
  case pivot:do(assignments, get, Req) of
    {ok, Assignments} when length(Assignments) > 0 ->
      case pivot:do(rewards, get, Req) of
        {ok, Reward} ->
          pivot:p([
            {arm_state, add, Req#pivot_req{bandit_arm = BanditArm, reward = Reward}}
          || BanditArm <- Assignments]),
          pivot:do_async(selections, renew, Req);
        _ ->
          noop
      end;
    _ ->
      noop
  end.
