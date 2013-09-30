-module(pivot).

% app api
-export([start/7]).
-export([stop/1]).

% client
-export([track/5]).

% server
-export([assign/4]).
-export([register/6]).

% api
-export([list/2]).
-export([report/3]).
-export([configure/4]).
-export([get_events/2]).
-export([get_event/3]).
-export([set_event/4]).

-define(DEFAULT_ALGORITHM, pivot_mab_ucb1).

-include("pivot.hrl").

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
  pivot_track:track(Ref, Env, App, Event, User).

% content owner

% -spec assign(ref(), app(), user()) -> [{bandit(), arm()}].

%new_assign(Env, Ref, App, User) ->
%  lookup_ref(ref_server:get(?MODULE, Ref, ref), #state{env = Env, app = App, user = User}, fun lookup_user_assignments/2).

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
register(Ref, Env, App, Bandit, Arms, Config) ->
  {ok, RefConf} = ref_server:get(?MODULE, Ref, ref),

  % Add the bandit to the app's list
  AppDB = RefConf#ref.app_db,
  ok = AppDB:add(Env, App, Bandit),

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
