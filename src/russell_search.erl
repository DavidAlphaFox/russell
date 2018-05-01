-module(russell_search).

-export([search/3]).

search(Stmt, State, Defs) ->
    search_stmt([Stmt], State, Defs).

search_stmt([Stmt|Stack], State = #{known := Known}, Defs) ->
    case maps:find(Stmt, Known) of
        {ok, N} ->
            {ok, N, State};
        error ->
            case lists:member(Stmt, Stack) of
                true ->
                    {not_found, State};
                false ->
                    search_stmt([Stmt|Stack], Defs, State, Defs)
            end
    end.

search_stmt(_, [], State, _) ->
    {not_found, State};
search_stmt([Stmt|_] = Stack, [{Name, {Ins, Out}}|T], State, Defs) ->
    case russell_core:match(Out, Stmt, #{}) of
        {error, _} ->
            search_stmt(Stack, T, State, Defs);
        {ok, Vars} ->
            case russell_core:subst(Ins, Vars, 0) of
                {Ins1, Vars, 0} ->
                    case search_stmts(Ins1, Stack, State, Defs) of
                        {not_found, _} ->
                            search_stmt(Stack, T, State, Defs);
                        {ok, Ins2, State1} ->
                            {N, State2} = add_stmt(Stmt, {{Name, 0}, Ins2}, State1),
                            {ok, N, State2}
                    end;
                _ ->
                    search_stmt(Stack, T, State, Defs)
            end
    end.

search_stmts([], _, State, _Defs) ->
    {ok, [], State};
search_stmts([H|T], Stack, State, Defs) ->
    case search_stmt([H|Stack], State, Defs) of
        {not_found, _} = NotFound ->
            NotFound;
        {ok, H1, State1} ->
            case search_stmts(T, Stack, State1, Defs) of
                {not_found, _} = NotFound ->
                    NotFound;
                {ok, T1, State2} ->
                    {ok, [H1|T1], State2}
            end
    end.

add_stmt(Stmt, Step, State = #{next_stmt := Next, known := Known, proven := Proven}) ->
    {Next, State#{next_stmt := Next+1, known := Known#{Stmt => Next}, proven := Proven#{Next => Step}}}.
