-module(pivot_user_db).

-type error() :: {error, any()}.
-type user() :: binary().
-type bandit() :: binary().
-type arm() :: binary().
-type assignment_date() :: calendar:datetime().
-type bandit_list() :: [{bandit(), arm(), assignment_date()}].

-callback list(User)
    -> {ok, bandit_list()}
    | error()
    when User::user().

-callback assign(User, Bandit, Arm, AssignmentDate)
    -> ok
    | error()
    when User::user(), Bandit::bandit(), Arm::arm(), AssignmentDate::assignment_date().
