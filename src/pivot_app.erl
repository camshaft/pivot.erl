-module(pivot_app).
-behaviour(application).

%% API.
-export([start/2]).
-export([stop/1]).

-include("pivot.hrl").

%% API.

start(_Type, _Args) ->
  ok = riakou:start_link(?PIVOT_GROUP, simple_env:get_binary("PIVOT_RIAK_URL")),
  throttle:start(),
  pivot_event_set:start(),

  pivot_sup:start_link().

stop(_State) ->
  ok.
