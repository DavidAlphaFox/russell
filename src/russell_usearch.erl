-module(russell_usearch).

-export([search/2]).

-export([contains_unbound/1]).

search(Stmts, Defs) ->
    case search_stmts(none, Stmts, Defs) of
        none ->
            [];
        Choices ->
            [C || C <- sets:to_list(Choices),
                  not contains_unbound(russell_unify:subst(Stmts, C))
            ]
    end.


search_stmts(Acc, [], _) ->
    Acc;
search_stmts(Acc, [H|T], Defs) ->
    search_stmts(product(Acc, search_stmt([H], Defs)), T, Defs).


search_stmt([Stmt|Stack], Defs) ->
    case lists:member(Stmt, Stack) of
        true ->
            none;
        false ->
            search_stmt(none, Defs, [Stmt|Stack], Defs)
    end.

search_stmt(Acc, [], _, _) ->
    Acc;
search_stmt(Acc, [H|T], Stack, Defs) ->
    search_stmt(union(search_stmt(H, Stack, Defs), Acc), T, Stack, Defs).


search_stmt({_, {Ins, Out}}, [Stmt|_]=Stack, Defs) ->
    case match_unify(Out, Stmt, #{}, #{}) of
        error ->
            none;
        {ok, Map, Subst} ->
            case normalize_subst(Subst) of
                error ->
                    none;
                {ok, Subst1} ->
                    Bound = maps:filter(fun(_, V) -> not contains_unbound(V) end, Subst1),

                    case contains_unbound(russell_unify:subst(Stmt, Subst1)) of
                        false ->
                            sets:from_list([Bound]);
                        true ->
                            case search_inputs(none, Ins, Map, Stack, Subst1, Defs) of
                                none ->
                                    case maps:size(Bound) of
                                        0 ->
                                            none;
                                        _ ->
                                            sets:from_list([Bound])
                                    end;
                                Choices ->
                                    sets:from_list(
                                      [ maps:merge(Bound, C)
                                        || C <- sets:to_list(Choices) ])
                            end
                    end
            end
    end.

search_inputs(Acc, [], _, _, _, _) ->
    Acc;
search_inputs(Acc, [H|T], Map, Stack, Subst, Defs) ->
    Choices = search_input(H, Map, Stack, Subst, Defs),
    search_inputs(product(Choices, Acc), T, Map, Stack, Subst, Defs).

search_input(Stmt, Map, Stack, Subst, Defs) ->
    case russell_def:subst(Stmt, Map, 0) of
        {Stmt1, Map, 0} ->
            Stmt2 = russell_unify:subst(Stmt1, Subst),
            case contains_unbound(Stmt2) of
                false ->
                    none;
                true ->
                    search_stmt([Stmt2|Stack], Defs)
            end;
        _ ->
            none
    end.


match_unify([], [], Map, Subst) ->
    {ok, Map, Subst};
match_unify([XH|XT], [YH|YT], Map, Subst) ->
    case match_unify(XH, YH, Map, Subst) of
        error ->
            error;
        {ok, Map1, Subst1} ->
            match_unify(XT, YT, Map1, Subst1)
    end;
match_unify({var, X}, Y, Map, Subst) ->
    case maps:find(X, Map) of
        error ->
            {ok, Map#{X => Y}, Subst};
        {ok, X1} ->
            case russell_unify:unify(X1, Y, Subst) of
                false ->
                    error;
                Subst1 ->
                    {ok, Map, Subst1}
            end
    end;
match_unify(X, {var, Y}, Map, Subst) when is_atom(X), is_integer(Y) ->
    case russell_unify:unify(X, {var, Y}, Subst) of
        false ->
            error;
        Subst1 ->
            {ok, Map, Subst1}
    end;
match_unify(X, X, Map, Subst) ->
    {ok, Map, Subst};
match_unify(_, _, _, _) ->
    error.

normalize_subst(Subst) ->
    Subst1 = russell_unify:compress(Subst),
    Bound = maps:filter(fun (K, _) -> not is_integer(K) end, Subst1),

    case lists:all(
           fun({var, I}) when is_integer(I) ->
                   true;
              (_) ->
                   false
           end,
           maps:values(Bound))
    of
        false ->
            error;
        true ->
            Subst2 =
                maps:merge(
                  maps:filter(fun(K, _) -> is_integer(K) end, Subst1),
                  maps:from_list([{I, {var,K}} || {K,{var,I}} <- maps:to_list(Bound)])),

            {ok, russell_unify:compress(Subst2)}
    end.


contains_unbound([]) ->
    false;
contains_unbound([H|T]) ->
    contains_unbound(H) or contains_unbound(T);
contains_unbound({var, X}) when is_integer(X) ->
    true;
contains_unbound(_) ->
    false.

union(none, S) ->
    S;
union(S, none) ->
    S;
union(A, B) ->
    sets:union(A,B).

product(none, S) ->
    S;
product(S, none) ->
    S;
product(A, B) ->
    sets:from_list(
      [ maps:merge(X,Y)
        || X <- sets:to_list(A),
           Y <- sets:to_list(B),
           maps:with(maps:keys(Y), X) =:= maps:with(maps:keys(X), Y)]).
