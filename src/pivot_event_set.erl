%%
%% pivot_event_set.erl
%%
-module(pivot_event_set).

-export([start/0]).
-export([get/1]).
-export([add/1]).
-export([delete/1]).
-export([encode/2]).
-export([decode/1]).

%% private
-export([clear_buffer/3]).

-include("pivot.hrl").

-define(BUFFER, pivot_event_set_buffer).
-define(BUFFER_RATE, 50).

-define(STATE_BUCKET(Env), <<"eset:", Env/binary>>).
-define(STATE_KEY(App, Version, BanditArm, EventSet), ?KEY_HASH(App, Version, BanditArm, EventSet)).

start() ->
  ets:new(?BUFFER, [bag, public, {write_concurrency, true}, named_table]).

add(#pivot_req{env = Env, app = App, version = Version, bandit_arm = BanditArm, event_set = Set, reward = Reward, token = Token}) ->
  Bucket = ?STATE_BUCKET(Env),
  Key = ?STATE_KEY(App, Version, BanditArm, Set),
  Value = encode(Token, Reward),
  true = ets:insert(?BUFFER, {{Bucket, Key}, Value}),
  throttle:exec(?MODULE, clear_buffer, [Bucket, Key, App], {Bucket, Key}, ?BUFFER_RATE, true).

clear_buffer(Bucket, Key, App) ->
  BucketType = {<<"set">>, Bucket},
  case get_or_create(BucketType, Key) of
    {ok, EventSet} ->
      case ets:lookup(?BUFFER, {Bucket, Key}) of
        [] ->
          {ok, riakc_set:size(EventSet)};
        Events ->
          io:format("measure#event_set.buffer=~pevents app=~s~n", [length(Events), App]),
          NewEventSet = add_values(EventSet, Events),
          case riakou:do(?EVENT_SET_GROUP, update_type, [BucketType, Key, riakc_set:to_op(NewEventSet), [create]]) of
            ok ->
              true = ets:delete(?BUFFER, {Bucket, Key}),
              {ok, riakc_set:size(NewEventSet)};
            Error ->
              Error
          end
      end;
    Error ->
      Error
  end.

add_values(EventSet, []) ->
  EventSet;
add_values(EventSet, [{_, Event}|Events]) ->
  add_values(riakc_set:add_element(Event, EventSet), Events).

get_or_create(Bucket, Key) ->
  case riakou:do(?EVENT_SET_GROUP, fetch_type, [Bucket, Key]) of
    {ok, EventSet} ->
      {ok, EventSet};
    {error, {notfound, _}} ->
      {ok, riakc_set:new()};
    Error ->
      Error
  end.

get(#pivot_req{env = Env, app = App, version = Version, bandit_arm = BanditArm, event_set = Set, count = Count, score = Score}) ->
  case riakou:do(?EVENT_SET_GROUP, fetch_type, [{<<"set">>, ?STATE_BUCKET(Env)}, ?STATE_KEY(App, Version, BanditArm, Set)]) of
    {ok, Obj} ->
      %% NOTE
      %% This is not sorting because the riakc_set is an ordset. This is an implementation
      %% detail and should be considered when updating the client library.
      EventsSet = riakc_set:value(Obj),
      {ok, chain(Count, Score, EventsSet)};
    {error, {notfound, _}} ->
      {error, notfound};
    Error ->
      Error
  end.

delete(#pivot_req{env = Env, app = App, version = Version, bandit_arm = BanditArm, event_set = Set}) ->
  riakou:do(?EVENT_SET_GROUP, delete, [{<<"set">>, ?STATE_BUCKET(Env)}, ?STATE_KEY(App, Version, BanditArm, Set)]).

chain(Count, Score, []) ->
  {Count, Score};
chain(Count, Score, [Event|Events]) ->
  N = Count + 1,
  NewScore = ((N - 1.0) / N) * Score + (1.0 / N) * decode(Event),
  chain(N, NewScore, Events).

id(Token) ->
  {Megasecs, Secs, Microsecs} = erlang:now(),
  Time = (((Megasecs * 1000000) + Secs) * 1000000) + Microsecs,
  Hash = erlang:phash2(Token),
  <<Time:64, Hash:32>>.

encode(Token, Reward) when is_float(Reward) ->
  encode(Token, float_to_binary(Reward, [{decimals, 10}, compact]));
encode(Token, Reward) ->
  <<(id(Token))/binary, Reward/binary>>.

decode(<<_:96, Reward/binary>>) ->
  binary_to_float(Reward).
