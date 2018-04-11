-module(russell_unify).

-export([unify/3, subst/2, compress/1]).

lookup(K, S) ->
    case maps:find(K, S) of
        error ->
            {var, K};
        {ok, V} ->
            V
    end.

lookup_var(V, S) ->
    case lookup(V, S) of
        {var, V1} when V1 =/= V ->
            lookup_var(V1, S);
        Other ->
            Other
    end.

subst({var, V}, S) ->
    case lookup_var(V, S) of
        {var, _} = V1 ->
            V1;
        Other ->
            subst(Other, S)
    end;
subst([], _S) ->
    [];
subst([H|T], S) ->
    [subst(H,S)|subst(T,S)];
subst(X, _) when is_atom(X) ->
    X.

occurs(X, {var, V}, S) ->
    case lookup_var(V, S) of
        {var, Y} ->
            X =:= Y;
        Other ->
            occurs(X, Other, S)
    end;
occurs(X, [H|T], S) ->
    occurs(X, H, S) or occurs(X, T, S);
occurs(_, _, _) ->
    false.

unify({var, V1}, Y, S) ->
    case lookup_var(V1, S) of
        {var, X} ->
            case occurs(X, Y, S) of
                true ->
                    case Y of
                        {var, V2} ->
                            case lookup_var(V2, S) of
                                {var, X} ->
                                    S;
                                _ ->
                                    false
                            end;
                        _ ->
                            false
                    end;
                false ->
                    S#{X => Y}
            end;
        Other ->
            unify(Other, Y, S)
    end;
unify(X, {var, _} = Y, S) ->
    unify(Y, X, S);
unify([], [], S) ->
    S;
unify([H1|T1], [H2|T2], S) ->
    case unify(H1, H2, S) of
    	false ->
    	    false;
    	S1 ->
    	    unify(T1, T2, S1)
    end;
unify(X, X, S) when is_atom(X) ->
    S;
unify(_, _, _) ->
    false.

compress(Subst) ->
    maps:map(
      fun(_, V) -> subst(V, Subst) end,
      Subst).
