-module(pivot_db).

-type error() :: {{error, any()}, conn()}.
-type bandit() :: binary().
-type arm() :: binary().
-type conn() :: term().
-type options() :: [{binary(), term()}].
-type count_diff() :: pos_integer().
-type score_diff() :: float().

%% Note:
% The score, when in conflict with other writes, proably should
% be averaged instead of all being applied - I'll have to do some
% looking into
-type patch() :: {arm(), count_diff(), score_diff()}.

-callback init(Opts)
    -> {ok, conn()}
    | {error, any()}
    when Opts::options().

-callback create(Name, Arms, Conn)
    -> {ok, conn()}
    | error()
    when Name::bandit(), Arms::[binary()], Conn::conn().

-callback bandits(Conn)
    -> {ok, [bandit()], Conn}
    | error()
    when Conn::conn().

-callback state(Bandit, Conn)
    -> {ok, pivot_mab:state(), conn()}
    | error()
    when Bandit::bandit(), Conn::conn().

-callback update(Bandit, Patches, Conn)
    -> {ok, conn()}
    | error()
    when Bandit::bandit(), Patches::[patch()], Conn::conn().
