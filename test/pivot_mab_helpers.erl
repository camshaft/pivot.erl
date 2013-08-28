-module(pivot_mab_helpers).

-export([generate/4]).

-include_lib("eunit/include/eunit.hrl").

generate(Mod, Opts, Runs, ArmMeans) ->
  fun () ->
    Config = Mod:init(Opts),
    Arms = [{Arm, bernoulli_arm(P)} || {Arm, P} <- ArmMeans],
    State = [{Arm, {0, 0.0}} || {Arm, _} <- ArmMeans],
    State2 = execute(Mod, Arms, State, Config, Runs),
    Counts = [{Arm, Count} || {Arm, {Count, _Score}} <- State2],
    ?assertEqual(find_winner(ArmMeans), find_winner(Counts)),
    State2
  end.

execute(_, _, State, _, 0) -> State;
execute(Mod, Arms, State, Config, Runs) ->
  {ok, Arm, State2} = Mod:select(State, Config),
  PullArm = proplists:get_value(Arm, Arms),
  {ok, State3} = Mod:update(Arm, PullArm(), State2, Config),
  execute(Mod, Arms, State3, Config, Runs-1).

find_winner(Counts) ->
  find_winner(Counts, {undefined, 0}).

find_winner([], {Winner, _}) ->
  Winner;
find_winner([{_, Count} = Arm|Counts], {_, WinnerCount} = Winner) ->
  case Count > WinnerCount of
    true -> find_winner(Counts, Arm);
    false -> find_winner(Counts, Winner)
  end.

bernoulli_arm(P) ->
  fun () ->
    case random:uniform() of
      N when N > P -> 0.0;
      _ -> 1.0
    end
  end.
