-module(pivot_track).

-export([track/5]).

-include("pivot.hrl").

-record(state, {
  env,
  app,
  event,
  user,
  assignments,
  reward,
  cardinality
}).

% -spec track(ref(), app(), event(), user()) -> ok.
track(Ref, Env, App, Event, User) ->
  lookup_ref(ref_server:get(pivot, Ref, ref), #state{env = Env, app = App, event = Event, user = User}).

lookup_ref({ok, Ref}, State) ->
  lookup_event(Ref, State);
lookup_ref(_, _) ->
  {error, unknown_ref}.

lookup_event(Ref = #ref{event_db = EventDB}, State = #state{env = Env, app = App, event = Event}) ->
  handle_event(EventDB:get(Env, App, Event), Ref, State).

handle_event({ok, Reward, Cardinality}, Ref, State) ->
  lookup_user_assignments(Ref, State#state{reward = Reward, cardinality = Cardinality});
handle_event({ok, Reward}, Ref, State) ->
  lookup_user_assignments(Ref, State#state{reward = Reward});
handle_event({error, notfound}, _, _) ->
  ok;
handle_event(Error, _, _) ->
  Error.

lookup_user_assignments(Ref = #ref{user_db = UserDB}, State = #state{env = Env, app = App, user = User}) ->
  case UserDB:assignments(Env, App, User) of
    {ok, Assignments} ->
      maybe_reward(Assignments, Ref, State);
    Error ->
      Error
  end.

%% TODO figure out bandit/arm expiration
%%   do we:
%%     save an expiration date?
%%     save the assigned date and then lookup the max length?

maybe_reward([{Bandit, Arm, Usages}|Assignments], Ref = #ref{user_db = UserDB}, State = #state{env = Env, app = App, user = User, cardinality = Cardinality}) when Cardinality =< Usages, is_integer(Cardinality) ->
  %% remove the assignment from the user - it's expired
  ok = UserDB:remove_assignment(Env, App, User, Bandit, Arm),
  reward({Bandit, Arm}, Ref, State),
  maybe_reward(Assignments, Ref, State);
maybe_reward([{Bandit, Arm, _Usages}|Assignments], Ref = #ref{user_db = UserDB}, State = #state{env = Env, app = App, user = User}) ->
  ok = UserDB:increment_usage(Env, App, User, Bandit, Arm),
  reward({Bandit, Arm}, Ref, State),
  maybe_reward(Assignments, Ref, State);
maybe_reward([{Bandit, Arm}|Assignments], Ref, State) ->
  reward({Bandit, Arm}, Ref, State),
  maybe_reward(Assignments, Ref, State);
maybe_reward([], _Ref, _State) ->
  ok.

reward({Bandit, Arm}, #ref{mab_state_db = MabStateDB}, #state{env = Env, app = App, reward = Reward})->
  MabStateDB:add_reward(Env, App, Bandit, Arm, Reward).
