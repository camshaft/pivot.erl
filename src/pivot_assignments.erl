%%
%% pivot_assignments.erl
%%
-module(pivot_assignments).

-export([assign/1]).
-export([get/1]).
-export([encode/1]).
-export([decode/1]).

-include("pivot.hrl").

-define(SALT_LENGTH, 4).

assign(Req) ->
  case pivot:do(selections, get, Req) of
    {ok, Selections} ->
      %% TODO get the app prefrence on the length of an assignment
      {ok, filter_superbandit(Selections), encode(Selections), 86400};
    Error ->
      Error
  end.

get(#pivot_req{token = Token, id = ReqID, app = App}) ->
  case catch decode(Token) of
    {'EXIT', _} ->
      io:format("count#invalid_token=1 token=~s app=~s request_id=~s~n", [Token, App, ReqID]),
      {error, invalid_token};
    Selections ->
      {ok, Selections}
  end.

filter_superbandit(Assignments) ->
  [BA || BA = {Bandit, _} <- Assignments, Bandit =/= ?SUPER_BANDIT].

encode(Selections) ->
  encode(Selections, []).

encode([], BanditArms) ->
  %% Generate a salt so no user gets the same token for the same assignments
  Salt = crypto:strong_rand_bytes(?SALT_LENGTH),
  Token = list_to_binary(BanditArms),
  websaferl:encode(<<(hmac(Salt, Token))/binary, Salt/binary, Token/binary>>);
encode([{Bandit, Arm}|Selections], Token) ->
  encode(Selections, [?BANDIT_ARM_HASH(Bandit, Arm)|Token]).

decode(HToken) ->
  <<HMAC:16/binary, Salt:?SALT_LENGTH/binary, Assignments/binary>> = websaferl:decode(HToken),
  HMAC = hmac(Salt, Assignments),
  decode(Assignments, []).

decode(<<>>, Assignments) ->
  Assignments;
decode(<<Assignment:?BANDIT_ARM_HASH_LENGTH/binary, Rest/binary>>, Assignments) ->
  decode(Rest, [Assignment|Assignments]).

hmac(Salt, Token) ->
  crypto:hmac(md5, simple_env:get_binary("ASSIGNMENTS_KEY"), [Salt, Token]).
