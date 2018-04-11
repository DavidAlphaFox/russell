-module(russell_usearch).

-export([search/3]).

-export([contains_unbound/1, normalize_subst/1]).

search(Stmts, Next, Defs) ->
    to_list(
      lists:foldl(
        fun(C, Acc) ->
                union(from_list(search_choice(C, Stmts, Next, Defs)), Acc)
        end,
        none,
        to_list(search_stmts(none, Stmts, Next, Defs)))).

search_choice(Subst, Stmts, Next, Defs) ->
    Stmts1 =
        [ S
          || S <- russell_unify:subst(Stmts, Subst),
             contains_unbound(S)],

    case Stmts1 of
        [] ->
            [Subst];
        _ ->
            [maps:merge(C, Subst) || C <- search(Stmts1, Next, Defs)]
    end.


search_stmts(Acc, [], _, _) ->
    Acc;
search_stmts(Acc, [H|T], Next, Defs) ->
    search_stmts(product(Acc, search_stmt([H], Next, Defs)), T, Next, Defs).


search_stmt([Stmt|Stack], Next, Defs) ->
    case lists:member(Stmt, Stack) of
        true ->
            none;
        false ->
            case contains_bound(Stmt) of
                false ->
                    none;
                true ->
                    Unbound = unbound_vars(Stmt),
                    Stack1 = [Stmt|Stack],

                    lists:foldl(
                      fun(X, Acc) ->
                              union(search_stmt(X, Stack1, Unbound, Next, Defs), Acc)
                      end,
                      none,
                      Defs)
            end
    end.


search_stmt({_, {Ins, Out}}, [Stmt|_]=Stack, Unbound, Next, Defs) ->
    {Out1, Map, Next1} = russell_def:subst(Out, #{}, Next),
    case russell_unify:unify(Out1, Stmt, #{}) of
        false ->
            none;
        Subst ->
            case normalize_subst(Subst) of
                error ->
                    none;
                {ok, Subst1} ->
                    Choices =
                        case contains_unbound(russell_unify:subst(Stmt, Subst1)) of
                            false ->
                                [Subst1];
                            true ->
                                to_list(
                                  lists:foldl(
                                    fun (X, Acc) ->
                                            case russell_def:subst(X, Map, 0) of
                                                {X1, _, 0} ->
                                                    X2 = russell_unify:subst(X1, Subst1),
                                                    case contains_unbound(X2) of
                                                        false ->
                                                            Acc;
                                                        true ->
                                                            product(search_stmt([X2|Stack], Next1, Defs), Acc)
                                                    end;
                                                _ ->
                                                    Acc
                                            end
                                    end,
                                    none,
                                    Ins))
                        end,

                    Choices1 =
                        [ maps:filter(
                            fun(_, V) ->
                                    not contains_unbound(V)
                            end,
                            maps:with(Unbound, russell_unify:compress(maps:merge(Subst1, C))))
                          || C <- Choices],

                    case [ C
                           || C <- Choices1,
                              maps:size(C) > 0]
                    of
                        [] ->
                            none;
                        Choices2 ->
                            sets:from_list(Choices2)
                    end
            end
    end.


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

unbound_vars(X) ->
    unbound_vars(X, []).

unbound_vars([], Acc) ->
    Acc;
unbound_vars([H|T], Acc) ->
    unbound_vars(T, unbound_vars(H, Acc));
unbound_vars({var, X}, Acc) when is_integer(X) ->
    [X|Acc];
unbound_vars(_, Acc) ->
    Acc.

contains_bound([]) ->
    false;
contains_bound([H|T]) ->
    contains_bound(H) or contains_bound(T);
contains_bound({var, X}) when is_atom(X) ->
    true;
contains_bound(_) ->
    false.

contains_unbound([]) ->
    false;
contains_unbound([H|T]) ->
    contains_unbound(H) or contains_unbound(T);
contains_unbound({var, X}) when is_integer(X) ->
    true;
contains_unbound(_) ->
    false.

from_list([]) ->
    none;
from_list(L) ->
    sets:from_list(L).

to_list(none) ->
    [];
to_list(S) ->
    sets:to_list(S).

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
