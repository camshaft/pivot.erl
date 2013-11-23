%% -*- coding: utf-8 -*-
-module(pivot).

-export([p/1]).
-export([do_p/1]).
-export([s/1]).
-export([do/3]).
-export([do_async/3]).
-export([new/1]).
% private
-export([noop/5]).
-export([bootstrap/0]).

-include("pivot.hrl").

-define(LOG(NS, Fun, Time, App, ReqID),
  io:format("measure#~p.~p=~pus count#req.~p.~p=1 app=~s request_id=~s~n", [NS, Fun, Time, NS, Fun, App, ReqID])).

%% TODO allow switching logging on/off
%% TODO use throttled io

% -define(LOG(NS, Fun, Time, App, ReqID),
%   (case Time > 20000 of
%     true ->
%       io:format("measure#~p.~p=~pms count#req.~p.~p=1 app=~s request_id=~s~n", [NS, Fun, Time/1000, NS, Fun, App, ReqID]);
%     _ ->
%       ok
%   end)).

% -undef(LOG).
% -define(LOG(NS, Fun, Time, App, ReqID), noop(NS, Fun, Time, App, ReqID)).

p(Funs) ->
  rpc:pmap({?MODULE, do_p}, [], Funs).

do_p({NS, Fun, Req}) ->
  case catch ?MODULE:do(NS, Fun, Req) of
    {'EXIT', {undef, _}} ->
      {error, {undef, NS, Fun}};
    {'EXIT', Error} ->
      {error, Error};
    Res ->
      Res
  end.

%% TODO make series dependencies
s(_Funs) ->
  ok.

new(Props) ->
  % new(Props, #pivot_req{}).
  new(Props, #pivot_req{
    id = <<"invalid-request-id">>,
    env = <<"production">>,
    version = <<"*">>
  }).

new([], Req) ->
  Req;
new([{id, V}|Props], Req) ->
  new(Props, Req#pivot_req{id = V});
new([{env, V}|Props], Req) ->
  new(Props, Req#pivot_req{env = V});
new([{app, V}|Props], Req) ->
  new(Props, Req#pivot_req{app = V});
new([{version, V}|Props], Req) ->
  new(Props, Req#pivot_req{version = V});
new([{token, V}|Props], Req) ->
  new(Props, Req#pivot_req{token = V});
new([{event, V}|Props], Req) ->
  new(Props, Req#pivot_req{event = V});
new([{bandit, V}|Props], Req) ->
  new(Props, Req#pivot_req{bandit = V});
new([{arm, V}|Props], Req) ->
  new(Props, Req#pivot_req{arm = V});
new([{arms, V}|Props], Req) ->
  new(Props, Req#pivot_req{arms = V});
new([{reward, V}|Props], Req) ->
  new(Props, Req#pivot_req{reward = V});
new([{selections, V}|Props], Req) ->
  new(Props, Req#pivot_req{selections = V});
new([{event_set, V}|Props], Req) ->
  new(Props, Req#pivot_req{event_set = V});
new([{count, V}|Props], Req) ->
  new(Props, Req#pivot_req{count = V});
new([{score, V}|Props], Req) ->
  new(Props, Req#pivot_req{score = V}).

do(NS, Fun, Req = #pivot_req{id = ReqID, app = App}) ->
  {Time, Res} = timer:tc(mod(NS), Fun, [Req]),
  ?LOG(NS, Fun, Time, App, ReqID),
  case Res of
    {error, no_connections} = E ->
      io:format("count#no_connections.~s.~s=1 app=~s request_id=~s~n", [NS, Fun, Req#pivot_req.app, Req#pivot_req.id]),
      E;
    Res2 ->
      Res2
  end.

do_async(NS, Fun, Req) ->
  %% TODO supervise this
  %% TODO if we've got a lot of backpressure let's block
  %% TODO allow for a priority setting based on {NS, Fun}
  spawn_link(?MODULE, do, [NS, Fun, Req]),
  ok.

%% TODO allow setting the resolution - maybe though an ets table?
mod(rewards) ->
  pivot_rewards;
mod(events) ->
  pivot_events;
mod(assignments) ->
  pivot_assignments;
mod(event_set) ->
  pivot_event_set;
mod(arm_state) ->
  pivot_arm_state;
mod(bandit_state) ->
  pivot_bandit_state;
mod(selections) ->
  pivot_selections;
mod(bandits) ->
  pivot_bandits;
mod(arms) ->
  pivot_arms;
mod(Mod) ->
  Mod.

noop(_, _, _, _, _) ->
  ok.

bootstrap() ->
  Req = new([]),
  p([
    {bandits, enable, Req#pivot_req{bandit = <<"bandit1">>}},
    {arms, enable, Req#pivot_req{bandit = <<"bandit1">>, arm = <<"arm1">>}},
    {arms, enable, Req#pivot_req{bandit = <<"bandit1">>, arm = <<"arm2">>}},
    {bandits, enable, Req#pivot_req{bandit = <<"bandit2">>}},
    {arms, enable, Req#pivot_req{bandit = <<"bandit2">>, arm = <<"arm1">>}},
    {arms, enable, Req#pivot_req{bandit = <<"bandit2">>, arm = <<"arm2">>}},
    {rewards, set, Req#pivot_req{reward = <<"1.0">>, event = <<"purchase">>}},
    {rewards, set, Req#pivot_req{reward = <<"0.0">>, event = <<"leave">>}}
  ]).
