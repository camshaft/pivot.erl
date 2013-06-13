-module(pivot).

-export ([assign/2]).
-export ([ping/2]).
-export ([event/3]).
-export ([timeout/1]).

% Client Interface

% -spec assign(user()) -> [{bandit(), arm()}].
assign(_Ref, _User) ->
  [].

% -spec ping(user()) -> ok.
ping(_Ref, _User) ->
  ok.

% -spec event(event(), user()) -> ok.
event(_Ref, _Event, _User) ->
  ok.

timeout(_Ref) ->
  60.

% Server Interface

% -spec set_event_reward(event(), reward()) -> ok.
% set_event_payout(_Event, _Payout) ->
%   ok.

% -spec register_bandit(bandit(), [arm()]) -> ok.
% register_bandit(_Bandit, _Arms) ->
%   ok.

% -spec bandit_stats(bandit()) -> [{arm(), score()}] | {error, notfound}.
% bandit_stats(_Bandit) ->
%   [].
