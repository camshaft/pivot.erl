-module(pivot).

% app api
-export([start/4]).
-export([stop/1]).

% client
-export([track/4]).

% server
-export([assign/3]).
-export([register/4]).

% api
-export([list/1]).
-export([report/2]).
-export([configure/3]).
-export([get_events/1]).
-export([set_event/3]).

% app api

start(Ref, UserDB, MabStateDB, MabConfigDB, EventDB, AppDB) ->
  ok = pivot_server:set(Ref, user_db, UserDB),
  ok = pivot_server:set(Ref, mab_state_db, MabStateDB),
  ok = pivot_server:set(Ref, mab_config_db, MabConfigDB),
  ok = pivot_server:set(Ref, event_db, EventDB),
  ok = pivot_server:set(Ref, app_db, AppDB),
  {ok, Ref}.

stop(_Ref) ->
  ok.

% client

% -spec track(ref(), app(), event(), user()) -> ok.
track(Ref, App, Event, User) ->
  % Lookup event reward from the event_db
  {ok, EventDB} = pivot_server:get(Ref, event_db),
  {ok, Reward} = EventDB:reward(App, Event),

  % Lookup all of the assigned user bandit arms from the user_db
  {ok, UserDB} = pivot_server:get(Ref, user_db),
  {ok, Assignments} = UserDB:assignments(App, User),

  % Save the rewards for the arms to the mab_state_db
  {ok, MabStateDB} = pivot_server:get(Ref, mab_state_db),
  BanditArmPairs = [{Bandit, Arm} || {Bandit, Arm, _Meta} <- Assignments],
  MabStateDB:update(App, Reward, BanditArmPairs).

% content owner

% -spec assign(ref(), app(), user()) -> [{bandit(), arm()}].
assign(Ref, App, User) ->
  % Lookup the current bandit assignments for the user in the user_db
  {ok, UserDB} = pivot_server:get(Ref, user_db),
  {ok, Assignments} = UserDB:assignments(App, User),

  % List the bandits for the app
  {ok, AppDB} = pivot_server:get(Ref, app_db),
  {ok, Bandits} = AppDB:bandits(App),

  % Fetch the bandit configs from the bandit_db (which algorithm, expiration, etc)
  {ok, MabConfigDB} = pivot_server:get(Ref, mab_config_db),
  {ok, Configs} = MabConfigDB:configs(App, Bandits),

  % If the user's bandit is still valid just return that
  % TODO

  % Make a bandit selection for the user
  % TODO
  EnabledBandit = hd(Bandits),

  % Filter any disabled arms for each of the bandits
  {ok, MabArmsDB} = pivot_server:get(Ref, mab_arms_db),
  {ok, EnabledArms} = MabArmsDB:enabled(App, EnabledBandit),

  % Get the bandit's algorithm
  {ok, Algorithm, AlgorithmConfig} = MabConfigDB:algorithm(App, Bandit),

  % Fetch the state from the mab_state_db for the bandits
  {ok, MabStateDB} = pivot_server:get(Ref, mab_state_db),
  {ok, State} = MabStateDB:get(App, EnabledBandit, EnabledArms),

  % Feed the state to the chosen algorithm
  {ok, SelectedArm} = Algorithm:select(State, AlgorithmConfig),

  % Save the chosen bandits/arms to the user_db
  % TODO
  ok = UserDB:set(App, User, [{EnabledBandit, SelectedArm, []}]),

  % Return the assigned arms
  [{EnabledBandit, SelectedArm}].

% -spec register(ref(), app(), bandit(), [{arm(), boolean()}], config()) -> ok.
register(Ref, App, Bandit, Arms, Config) ->
  % Add the bandit to the app's list
  {ok, AppDB} = pivot_server:get(Ref, app_db),
  ok = AppDB:add(App, Bandit),

  % Initialize the state in mab_state_db
  {ok, MabStateDB} = pivot_server:get(Ref, mab_state_db),
  ok = MabStateDB:init(App, Bandit),

  % Set the arms for the bandit in the mab_arms_db
  {ok, MabArmsDB} = pivot_server:get(Ref, mab_arms_db),
  ok = MabArmsDB:set(App, Bandit, Arms),

  % Set the config for the mab
  {ok, MabConfigDB} = pivot_server:get(Ref, mab_config_db),
  ok = MabConfigDB:set(App, Bandit, Config),

  ok.

% api

list(Ref, App) ->
  % List all of the bandits for an app
  {ok, AppDB} = pivot_server:get(Ref, app_db),
  AppDB:bandits(App).

% -spec report(ref(), app(), bandit()) -> {confidence(), plays(), [{arm(), score()}]} | {error, notfound}.
report(Ref, App, Bandit) ->
  % Set the arms for the bandit in the mab_arms_db
  {ok, MabArmsDB} = pivot_server:get(Ref, mab_arms_db),
  {ok, Arms} = MabArmsDB:all(App, Bandit),

  % Fetch the bandit state from the mab_db
  {ok, MabStateDB} = pivot_server:get(Ref, mab_state_db),
  {ok, State} = MabStateDB:get(App, Bandit, Arms),

  % Calculate the confidence of the tests and return the report
  % TODO

  {0, 0, []}.

%% TODO set the default arm
%% TODO set the MAB algorithm
%% TODO set the arm expiration (how long an arm is assigned to a user)
configure(Ref, App, Bandit, Config) ->
  {ok, MabConfigDB} = pivot_server:get(Ref, mab_config_db),
  MabConfigDB:set(App, Bandit, Config).

% -spec get_event_reward(ref()) -> [{event(), reward()}].
get_events(Ref, App) ->
  {ok, EventDB} = pivot_server:get(Ref, event_db),
  EventDB:events(App).

% -spec set_event(ref(), event(), reward()) -> ok.
set_event(Ref, App, Event, Reward) ->
  {ok, EventDB} = pivot_server:get(Ref, event_db),
  EventDB:set_reward(App, Event, Reward).

%% TODO default arm expiration
