-module(pivot).

% app api
-export([start/7]).
-export([stop/1]).

% client
-export([track/5]).

% server
-export([assign/4]).
-export([register/5]).

% api
-export([list/2]).
-export([report/3]).
-export([configure/4]).
-export([get_events/2]).
-export([get_event/3]).
-export([set_event/4]).

-define(DEFAULT_ALGORITHM, pivot_mab_ucb1).

-record(ref, {
  user_db,
  mab_arms_db,
  mab_state_db,
  mab_config_db,
  event_db,
  app_db
}).

-record(track, {
  env,
  app,
  event,
  user,
  reward,
  cardinality
}).

% app api

start(Ref, UserDB, MabArmsDB, MabStateDB, MabConfigDB, EventDB, AppDB) ->
  require([ref_server]),
  ok = ref_server:set(?MODULE, Ref, ref, #ref{
    user_db = UserDB,
    mab_arms_db = MabArmsDB,
    mab_state_db = MabStateDB,
    mab_config_db = MabConfigDB,
    event_db = EventDB,
    app_db = AppDB
  }),
  {ok, Ref}.

stop(_Ref) ->
  ok.

% client

% -spec track(ref(), app(), event(), user()) -> ok.
track(Ref, Env, App, Event, User) ->
  lookup_ref(ref_server:get(?MODULE, Ref, ref), #track{env = Env, app = App, event = Event, user = User}, fun lookup_event/2).

lookup_ref({ok, Ref}, Track, Next) ->
  Next(Ref, Track);
lookup_ref(_, _, _) ->
  {error, unknown_ref}.

lookup_event(Ref = #ref{event_db = EventDB}, Track = #track{env = Env, app = App, event = Event}) ->
  handle_event(EventDB:get(Env, App, Event), Ref, Track).

handle_event({ok, Reward, Cardinality}, Ref, Track) ->
  lookup_user_assignments(Ref, Track#track{reward = Reward, cardinality = Cardinality});
handle_event({ok, Reward}, Ref, Track) ->
  lookup_user_assignments(Ref, Track#track{reward = Reward});
handle_event({error, notfound}, _, _) ->
  ok;
handle_event(Error, _, _) ->
  Error.

lookup_user_assignments(Ref = #ref{user_db = UserDB}, Track = #track{env = Env, app = App, user = User}) ->
  handle_user_assignments(UserDB:assignments(Env, App, User), Ref, Track).

handle_user_assignments({ok, Assignments}, Ref, Track) ->
  maybe_reward(Assignments, Ref, Track);
handle_user_assignments(Error, _, _) ->
  Error.

%% TODO figure out bandit/arm expiration
%%   do we:
%%     save an expiration date?
%%     save the assigned date and then lookup the max length?

maybe_reward([{Bandit, Arm, Usages}|Assignments], Ref = #ref{user_db = UserDB}, Track = #track{env = Env, app = App, user = User, cardinality = Cardinality}) when Cardinality =< Usages, is_integer(Cardinality) ->
  %% remove the assignment from the user - it's expired
  ok = UserDB:remove_assignment(Env, App, User, Bandit, Arm),
  reward({Bandit, Arm}, Ref, Track),
  maybe_reward(Assignments, Ref, Track);
maybe_reward([{Bandit, Arm, _Usages}|Assignments], Ref = #ref{user_db = UserDB}, Track = #track{env = Env, app = App, user = User}) ->
  ok = UserDB:increment_usage(Env, App, User, Bandit, Arm),
  reward({Bandit, Arm}, Ref, Track),
  maybe_reward(Assignments, Ref, Track);
maybe_reward([{Bandit, Arm}|Assignments], Ref, Track) ->
  reward({Bandit, Arm}, Ref, Track),
  maybe_reward(Assignments, Ref, Track);
maybe_reward([], _Ref, _Track) ->
  ok.

reward({Bandit, Arm}, #ref{mab_state_db = MabStateDB}, #track{env = Env, app = App, reward = Reward})->
  MabStateDB:add_reward(Env, App, Bandit, Arm, Reward).


% content owner

% -spec assign(ref(), app(), user()) -> [{bandit(), arm()}].
assign(Env, Ref, App, User) ->
  % Lookup the current bandit assignments for the user in the user_db
  {ok, UserDB} = ref_server:get(?MODULE, Ref, user_db),
  {ok, _Assignments} = UserDB:assignments(Env, App, User),

  % List the bandits for the app
  {ok, AppDB} = ref_server:get(?MODULE, Ref, app_db),
  {ok, Bandits} = AppDB:bandits(Env, App),

  % Fetch the bandit configs from the bandit_db (which algorithm, expiration, etc)
  {ok, MabConfigDB} = ref_server:get(?MODULE, Ref, mab_config_db),
  {ok, Configs} = MabConfigDB:configs(Env, App, Bandits),

  % If the user's bandit selection is still valid just return that
  % TODO

  % Make a bandit selection for the user
  % TODO
  EnabledBandit = hd(Bandits),
  {_, BanditConfig} = hd(Configs),

  Algorithm = fast_key:get(algorithm, BanditConfig, ?DEFAULT_ALGORITHM),
  AlgorithmConfig = fast_key:get(algorithm_config, BanditConfig, []),

  % Filter any disabled arms for each of the bandits
  {ok, MabArmsDB} = ref_server:get(?MODULE, Ref, mab_arms_db),
  {ok, EnabledArms} = MabArmsDB:enabled(Env, App, EnabledBandit),

  % Fetch the state from the mab_state_db for the bandits
  {ok, MabStateDB} = ref_server:get(?MODULE, Ref, mab_state_db),
  {ok, State} = MabStateDB:get(Env, App, EnabledBandit, EnabledArms),

  % Feed the state to the chosen algorithm
  {ok, SelectedArm, _NewState} = Algorithm:select(State, AlgorithmConfig),

  % Save the chosen bandits/arms to the user_db
  % TODO
  ok = UserDB:set(Env, App, User, [{EnabledBandit, SelectedArm, []}]),

  % Return the assigned arms
  {ok, [{EnabledBandit, SelectedArm}]}.

% -spec register(ref(), app(), bandit(), [{arm(), boolean()}], config()) -> ok.
register(Ref, App, Bandit, Arms, Config) ->
  % Add the bandit to the app's list
  {ok, AppDB} = ref_server:get(?MODULE, Ref, app_db),
  ok = AppDB:add(App, Bandit),

  % Set the config for the mab
  {ok, MabConfigDB} = ref_server:get(?MODULE, Ref, mab_config_db),
  ok = MabConfigDB:set(App, Bandit, Config),

  % Initialize the state in mab_state_db
  Algorithm = fast_key:get(algorithm, Config, ?DEFAULT_ALGORITHM),
  AlgorithmConfig = fast_key:get(algorithm_config, Config, []),
  {ok, InitialState} = Algorithm:init(AlgorithmConfig),

  {ok, MabStateDB} = ref_server:get(?MODULE, Ref, mab_state_db),
  ok = MabStateDB:init(App, Bandit, InitialState),

  % Set the arms for the bandit in the mab_arms_db
  {ok, MabArmsDB} = ref_server:get(?MODULE, Ref, mab_arms_db),
  MabArmsDB:set(App, Bandit, Arms).

% api

list(Ref, App) ->
  % List all of the bandits for an app
  {ok, AppDB} = ref_server:get(?MODULE, Ref, app_db),
  AppDB:bandits(App).

% -spec report(ref(), app(), bandit()) -> {ok, plays(), [{arm(), score()}]} | {error, notfound}.
report(Ref, App, Bandit) ->
  % Set the arms for the bandit in the mab_arms_db
  {ok, MabArmsDB} = ref_server:get(?MODULE, Ref, mab_arms_db),
  {ok, Arms} = MabArmsDB:all(App, Bandit),

  % Fetch the bandit state from the mab_db
  {ok, MabStateDB} = ref_server:get(?MODULE, Ref, mab_state_db),
  {ok, State} = MabStateDB:get(App, Bandit, Arms),

  % Fetch the config for the algorithm
  {ok, MabConfigDB} = ref_server:get(?MODULE, Ref, mab_config_db),
  {ok, Config} = MabConfigDB:config(App, Bandit),

  Algorithm = fast_key:get(algorithm, Config, ?DEFAULT_ALGORITHM),
  AlgorithmConfig = fast_key:get(algorithm_config, Config, []),

  % Calculate the confidence of the tests and return the report
  Algorithm:report(State, AlgorithmConfig).

%% TODO set the default arm
%% TODO set the MAB algorithm
%% TODO set the arm expiration (how long an arm is assigned to a user)
configure(Ref, App, Bandit, Config) ->
  {ok, MabConfigDB} = ref_server:get(?MODULE, Ref, mab_config_db),
  MabConfigDB:set(App, Bandit, Config).

% -spec get_event_reward(ref(), app()) -> [{event(), reward()}].
get_events(Ref, App) ->
  {ok, EventDB} = ref_server:get(?MODULE, Ref, event_db),
  EventDB:all(App).

% -spec get_event(ref(), event()) -> ok.
get_event(Ref, App, Event) ->
  {ok, EventDB} = ref_server:get(?MODULE, Ref, event_db),
  EventDB:get(App, Event).

% -spec set_event(ref(), event(), reward()) -> ok.
set_event(Ref, App, Event, Reward) ->
  {ok, EventDB} = ref_server:get(?MODULE, Ref, event_db),
  EventDB:set(App, Event, Reward).

%% @doc Start the given applications if they were not already started.
-spec require(list(module())) -> ok.
require([]) ->
  ok;
require([App|Tail]) ->
  case application:start(App) of
    ok -> ok;
    {error, {already_started, App}} -> ok
  end,
  require(Tail).
