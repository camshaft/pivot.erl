-module(throttle).

-export([start/0]).
-export([exec/5]).
-export([exec/6]).

%% private
-export([clear/4]).
-export([check_flag/4]).

-define(IN_FLIGHT, rl_inflight).
-define(RAISE_FLAG, rl_flags).

start() ->
  ets:new(?IN_FLIGHT, [set, public, named_table, {write_concurrency, true}]),
  ets:new(?RAISE_FLAG, [set, public, named_table, {write_concurrency, true}]).

exec(Mod, Fun, Args, Key, Rate) ->
  ?MODULE:exec(Mod, Fun, Args, Key, Rate, false).

exec(Mod, Fun, Args, Key, Rate, CheckFlag) ->
  EKey = {Key, Mod, Fun},
  case ets:insert_new(?IN_FLIGHT, {EKey, true}) of
    false when CheckFlag ->
      ets:insert_new(?RAISE_FLAG, {EKey, true}),
      ok;
    false ->
      ok;
    _ ->
      true = ets:delete(?RAISE_FLAG, EKey),
      try
        apply(Mod, Fun, Args)
      catch
        Error ->
          Error
      after
        case Rate > 0 of
          true ->
            timer:apply_after(Rate, ?MODULE, clear, [EKey, Args, Rate, CheckFlag]);
          _ ->
            clear(EKey, Args, Rate, CheckFlag)
        end
      end
  end.

clear(Key, Args, Rate, CheckFlag) ->
  true = ets:delete(?IN_FLIGHT, Key),
  %% wait for someone else to actually pick it up
  timer:sleep(Rate),
  check_flag(Key, Args, Rate, CheckFlag).

check_flag({Key, Mod, Fun} = EKey, Args, Rate, true) ->
  case ets:member(?RAISE_FLAG, EKey) of
    true ->
      ?MODULE:exec(Mod, Fun, Args, Key, Rate, true);
    _ ->
      ok
  end;
check_flag(_, _, _, _) ->
  ok.
