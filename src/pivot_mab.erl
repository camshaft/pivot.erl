-module(pivot_mab).

-type arm() :: binary().
-type options() :: [{binary(), term()}].
-type state() :: [{arm(), {pos_integer(), float()}}].
-type config() :: term().
%% TODO can we specify a float range? needs to be 0.0..1.0
-type reward() :: float().

-export_type([
  arm/0,
  options/0,
  state/0,
  config/0,
  reward/0
]).

-callback init(Opts)
    -> {ok, config()}
    | {error, any()}
    when Opts::options().

-callback select(State, Config)
    -> {ok, arm(), state()}
    | {error, any()}
    when State::state(), Config::config().

-callback update(Arm, Reward, State, Config)
    -> {ok, State}
    | {error, any()}
    when Arm::arm(), Reward::reward(), State::state(), Config::config().
