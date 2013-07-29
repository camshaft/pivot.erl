-module(pivot_mab_db).

-type error() :: {error, any()}.
-type bandit() :: binary().
-type arm() :: binary().
-type count_diff() :: pos_integer().
-type score_diff() :: float().

%% Note:
% The score, when in conflict with other writes, proably should
% be averaged instead of all being applied - I'll have to do some
% looking into
-type patch() :: {arm(), count_diff(), score_diff()}.

-callback register(Name, Arms, WIP)
    -> ok
    | error()
    when Name::bandit(), Arms::[binary()], WIP::boolean().

-callback list()
    -> {ok, [bandit()]}
    | error().

-callback report(Bandit)
    -> {ok, pivot_mab:state()}
    | error()
    when Bandit::bandit().

-callback update(Bandit, Patches)
    -> ok
    | error()
    when Bandit::bandit(), Patches::[patch()].
