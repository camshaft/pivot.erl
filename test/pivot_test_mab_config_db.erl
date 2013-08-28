-module(pivot_test_mab_config_db).

-export([set/3]).
-export([configs/2]).
-export([config/2]).

set(_App, _Bandit, _Config) ->
  ok.

configs(App, Bandits) ->
  {ok, [{Bandit, case config(App, Bandit) of {ok, Conf} -> Conf end} || Bandit <- Bandits]}.

config(_App, <<"bandit-1">>) ->
  {ok, []};
config(_App, <<"bandit-2">>) ->
  {ok, []};
config(_App, <<"bandit-3">>) ->
  {ok, []};
config(_App, _Key) ->
  {ok, []}.
