-module(pivot).

% app api
-export([start/1]).
-export([stop/1]).

% client
-export([track/4]).

% server
-export([assign/3]).
-export([register/3]).

% api
-export([list/1]).
-export([report/2]).
-export([configure/3]).
-export([get_events/1]).
-export([set_event/3]).

% app api

start(_Ref) ->
  ok.

stop(_Ref) ->
  ok.

% client

% -spec track(ref(), env(), event(), user()) -> ok.
track(_Ref, _Env, _Event, _User) ->
  ok.

% content owner

% -spec assign(ref(), env(), user()) -> [{bandit(), arm()}].
assign(_Ref, _Env, _User) ->
  [].

% -spec register(ref(), bandit(), [arm()]) -> ok.
register(_Ref, _Bandit, _Arms) ->
  ok.

% api

list(_Ref) ->
  [].

% -spec report(ref(), bandit()) -> {confidence(), plays(), [{arm(), score()}]} | {error, notfound}.
report(_Ref, _Bandit) ->
  {0, 0, []}.

%% TODO set the default arm
%% TODO set the MAB algorithm
%% TODO set the arm expiration (how long an arm is assigned to a user)
configure(_Ref, _Bandit, _Conf) ->
  ok.

% -spec get_event_reward(ref()) -> [{event(), reward()}].
get_events(_Ref) ->
  [].

% -spec set_event(ref(), event(), reward()) -> ok.
set_event(_Ref, _Event, _Reward) ->
  ok.

%% TODO max concurrent tests
%% TODO default arm expiration
