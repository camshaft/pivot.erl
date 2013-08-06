-module(pivot_mab_db).

-type error() :: {error, any()}.
-type app() :: binary().
-type bandit() :: binary().
-type arm() :: binary().
-type count_diff() :: pos_integer().
-type score_diff() :: float().

%% Note:
% The score, when in conflict with other writes, proably should
% be averaged instead of all being applied - I'll have to do some
% looking into
-type patch() :: {arm(), count_diff(), score_diff()}.

-callback register(App, Bandit, Arms, WIP)
    -> ok
    | error()
    when App::app(), Bandit::bandit(), Arms::[binary()], WIP::boolean().

-callback list(App)
    -> {ok, [bandit()]}
    | error()
    when App::app().

-callback report(App, Bandit)
    -> {ok, pivot_mab:state()}
    | error()
    when App:app(), Bandit::bandit().

-callback update(App, Bandit, Patches)
    -> ok
    | error()
    when App:app(), Bandit::bandit(), Patches::[patch()].
