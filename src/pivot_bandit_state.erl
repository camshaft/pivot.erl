%% -*- coding: utf-8 -*-
%%
%% pivot_bandit_state.erl
%%
-module(pivot_bandit_state).

-export([get/1]).
-export([select/1]).

-include("pivot.hrl").

get(Req = #pivot_req{arms = undefined}) ->
  case pivot:do(arms, enabled, Req) of
    {ok, Arms} ->
      ?MODULE:get(Req#pivot_req{arms = Arms});
    Error ->
      Error
  end;
get(#pivot_req{arms = []}) ->
  {ok, []};
get(Req = #pivot_req{arms = Arms}) ->
  {ok, pivot:p([
    {arm_state, get, Req#pivot_req{arms = undefined, arm = Arm}}
  || Arm <- Arms])}.

select(Req) ->
  case ?MODULE:get(Req) of
    {ok, []} ->
      {error, no_selection};
    {ok, State} ->
      {ok, Arm} = select(State, Req#pivot_req.explore),
      {ok, Req#pivot_req.bandit, Arm};
    Error ->
      Error
  end.

select(State, Explore) ->
  maybe_prepare_state(State, 0, State, Explore).

% Return arms that haven't been tried yet and total the number of arms
maybe_prepare_state([{ok, Arm, {0, _Score}}|_Arms], _TotalCount, _State, _) ->
  {ok, Arm};
maybe_prepare_state([{ok, _Arm, {Count, _Score}}|Arms], TotalCount, State, Explore) ->
  maybe_prepare_state(Arms, TotalCount + Count, State, Explore);
maybe_prepare_state([{error, no_connections}|Arms], TotalCount, State, Explore) ->
  maybe_prepare_state(Arms, TotalCount, State, Explore);
maybe_prepare_state([], TotalCount, State, Explore) ->
  choose_best_arm(TotalCount, {undefined, -1}, State, State, Explore).

% Iterate a list of arms and pick the highest
choose_best_arm(TotalCount, {_HighArm, HighScore} = High, [{ok, Arm, {Count, Score}}|Arms], State, Explore) ->
  case calculate_score(TotalCount, Count, Score, Explore) of
    NewScore when NewScore > HighScore ->
      choose_best_arm(TotalCount, {Arm, NewScore}, Arms, State, Explore);
    _ ->
      choose_best_arm(TotalCount, High, Arms, State, Explore)
  end;
choose_best_arm(TotalCount, High, [{error, no_connections}|Arms], State, Explore) ->
  choose_best_arm(TotalCount, High, Arms, State, Explore);
choose_best_arm(_, {HighArm, _}, [], _State, _) ->
  {ok, HighArm}.

calculate_score(TotalCount, Count, Score, true) ->
  Score + math:sqrt((2 * math:log(TotalCount)) / Count);
calculate_score(TotalCount, Count, Score, _) ->
  Score + math:sqrt((math:log(TotalCount)) / Count).
