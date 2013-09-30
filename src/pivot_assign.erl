-module(pivot_assign).

-export([assign/4]).

-include("pivot.hrl").

-record(state, {
  env,
  app,
  user,
  assignments,
  reward,
  cardinality
}).

assign(Env, Ref, App, User) ->
  lookup_ref(ref_server:get(pivot, Ref, ref), #state{env = Env, app = App, user = User}).

lookup_ref({ok, Ref}, State) ->
  lookup_bandits(Ref, State);
lookup_ref(_, _) ->
  {error, unknown_ref}.

lookup_bandits(Ref = #ref{app_db = AppDB}, State = #state{env = Env, app = App}) ->
  handle_bandits(AppDB:bandits(Env, App), Ref, State).

handle_bandits(_, _, _) ->
  ok.
