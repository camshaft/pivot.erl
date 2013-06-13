-module(pivot_db).

-type error() :: {{error, any()}, conn()}.
-type bandit() :: binary().
-type conn() :: term().
-type options() :: [{binary(), term()}].

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

-callback update(Bandit, Patch, Conn)
    -> {ok, conn()}
    | error()
    when Bandit::bandit(), Patch::pivot_mab:state(), Conn::conn().
