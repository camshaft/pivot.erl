-module(pivot_mab_ucb1_test).

-include_lib("eunit/include/eunit.hrl").

simple_test_() ->
  pivot_mab_helpers:generate(pivot_mab_ucb1, [], 1000, [
    {<<"arm 1">>, 0.1},
    {<<"arm 2">>, 0.1},
    {<<"arm 3">>, 0.9}
  ]).

many_arm_test_() ->
  pivot_mab_helpers:generate(pivot_mab_ucb1, [], 10000, [
    {<<"arm 1">>, 0.1},
    {<<"arm 2">>, 0.1},
    {<<"arm 3">>, 0.9},
    {<<"arm 4">>, 0.1},
    {<<"arm 5">>, 0.1},
    {<<"arm 6">>, 0.1},
    {<<"arm 7">>, 0.1},
    {<<"arm 8">>, 0.1},
    {<<"arm 9">>, 0.1},
    {<<"arm 10">>, 0.1},
    {<<"arm 11">>, 0.1},
    {<<"arm 12">>, 0.1},
    {<<"arm 13">>, 0.9},
    {<<"arm 14">>, 0.1},
    {<<"arm 15">>, 0.1},
    {<<"arm 16">>, 0.1},
    {<<"arm 17">>, 0.1},
    {<<"arm 18">>, 0.1},
    {<<"arm 19">>, 0.1},
    {<<"arm 20">>, 0.1}
  ]).
