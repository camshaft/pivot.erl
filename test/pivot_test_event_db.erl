-module(pivot_test_event_db).

-export([all/1]).
-export([get/3]).
-export([set/4]).

all(_App) ->
  {ok, [{<<"high-event">>, 1.0}, {<<"medium-event">>, 0.5}, {<<"low-event">>, 0.1}]}.

get(_, _, <<"high-event">>) ->
  {ok, 1.0};
get(_, _, <<"medium-event">>) ->
  {ok, 0.5};
get(_, _, <<"low-event">>) ->
  {ok, 0.1};
get(_, _, _) ->
  {ok, 0.0}.

set(_, _App, _Event, _Reward) ->
  ok.
