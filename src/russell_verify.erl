-module(russell_verify).

-export(
   [verify_proof/2, format_error/1,
    format_tokens/1, format_token/1]).

format_tokens(Tokens) ->
    string:join([format_token(T) || T <- Tokens], " ").

format_token(List) when is_list(List) ->
    ["(", format_tokens(List), ")"];
format_token({var, V}) when is_atom(V) ->
    io_lib:format(".~ts", [V]);
format_token({var, V}) when is_integer(V) ->
    io_lib:format(".(~B)", [V]);
format_token(A) ->
    io_lib:format("~ts", [A]).

format_error({def_not_found, Name}) ->
    io_lib:format("definition ~ts not found", [Name]);
format_error({input_number_mismatch, E, G}) ->
    io_lib:format("input number mismatch, expected ~B, got ~B", [E,G]);
format_error({not_match, E, G}) ->
    io_lib:format(
      "statement mismatch~n  Expected:~n    ~ts~n  Got:~n    ~ts",
      [format_tokens(E),format_tokens(G)]);
format_error({not_match, E, G, V, X}) ->
    io_lib:format(
      "statement mismatch~n  Expected:~n     ~ts~n  Where ~ts =~n    ~ts~n  Got:~n    ~ts",
      [format_tokens(E),V,format_token(X),format_tokens(G)]).


find({Name, Line}, Defs) ->
    case maps:find(Name, Defs) of
        error ->
            {error, {Line, ?MODULE, {def_not_found, Name}}};
        {ok, Def} ->
            {ok, Def}
    end.

find_exact({_, Line}=Name, NIn, Defs) ->
    case find(Name, Defs) of
        {error, _} = Error ->
            Error;
        {ok, {In, _}} when length(In) =/= NIn ->
            {error, {Line, ?MODULE, {input_number_mismatch, length(In), NIn}}};
        {ok, Def} ->
            {ok, Def}
    end.

subst([], Vars, Counter) ->
    {[], Vars, Counter};
subst([H|T], Vars, Counter) ->
    {H1, Vars1, Counter1} = subst(H, Vars, Counter),
    {T1, Vars2, Counter2} = subst(T, Vars1, Counter1),
    {[H1|T1], Vars2, Counter2};
subst({var, V}, Vars, Counter) ->
    case maps:find(V, Vars) of
        error ->
            {{var, Counter}, Vars#{V => {var, Counter}}, Counter+1};
        {ok, Value} ->
            {Value, Vars, Counter}
    end;
subst(A, Vars, Counter) ->
    {A, Vars, Counter}.

match([], [], Vars) ->
    {ok, Vars};
match([H1|T1], [H2|T2], Vars) ->
    case match(H1, H2, Vars) of
        {error, _} = Error ->
            Error;
        {ok, Vars1} ->
            match(T1, T2, Vars1)
    end;
match({var, V}, X, Vars) ->
    case maps:find(V, Vars) of
        error ->
            {ok, Vars#{V => X}};
        {ok, X} ->
            {ok, Vars};
        {ok, X1} ->
            {error, {not_match, V, X1}}
    end;
match(X, X, Vars) ->
    {ok, Vars};
match(_, _, _) ->
    {error, not_match}.

match_stmt({{_,Line}, Stmt}, In, Vars) ->
    case match(In, Stmt, Vars) of
        {error, not_match} ->
            {error, {Line,?MODULE,{not_match, In, Stmt}}};
        {error, {not_match, V, X}} ->
            {error, {Line,?MODULE,{not_match, In, Stmt, V, X}}};
        {ok, Vars1} ->
            {ok, Vars1}
    end.

match_stmts([], [], Vars) ->
    {ok, Vars};
match_stmts([H1|T1],[H2|T2],Vars) ->
    case match_stmt(H1,H2,Vars) of
        {error, _} = Error ->
            Error;
        {ok, Vars1} ->
            match_stmts(T1,T2,Vars1)
    end.

verify_step(InStmts, {In, Out}, C) ->
    case match_stmts(InStmts, In, #{}) of
        {error, _} = Error ->
            Error;
        {ok, Vars} ->
            {Stmt, _, C1} = subst(Out, Vars, C),
            {ok, Stmt, C1}
    end.

verify_step(Name, InNames, Stmts, Counter, Defs) ->
    case find_exact(Name, length(InNames), Defs) of
        {error, _} = Error ->
            Error;
        {ok, Def} ->
            InStmts =
                [ {{N,L}, maps:get(N,Stmts)}
                  || {N,L} <- InNames],
            verify_step(InStmts, Def, Counter)
    end.

verify_steps([], _, _, _) ->
    {ok, []};
verify_steps([{Name, [{OutName, _}=Out|InNames]}|T], Stmts, Counter, Defs) ->
    case verify_step(Name, InNames, Stmts, Counter, Defs) of
        {error, Error} ->
            {error, Error, []};
        {ok, OutStmt, Counter1} ->
            Step = {{Name, InNames}, {Out, OutStmt}},
            case verify_steps(T, Stmts#{OutName => OutStmt}, Counter1, Defs) of
                {error, Error, Steps} ->
                    {error, Error, [Step|Steps]};
                {ok, Steps} ->
                    {ok, [Step|Steps]}
            end
    end.

verify_output(Ins, Out, OutStmt) ->
    InStmts = [I || {_,I} <- Ins],
    match_stmts(Ins ++ [Out], InStmts ++ [OutStmt], #{}).

verify_proof({Name, InNames, Steps}, Defs) ->
    case find_exact(Name, length(InNames), Defs) of
        {error, _} = Error ->
            Error;
        {ok, {InStmts, OutStmt}} ->
            InStmts1 = lists:zip(InNames, InStmts),
            case verify_steps(Steps, maps:from_list([{N,S} || {{N,_},S} <- InStmts1]), 0, Defs) of
                {error, Error, Log} ->
                    {error, Error, InStmts1, Log};
                {ok, Log} ->
                    {_, Out} = lists:last(Log),
                    case verify_output(InStmts1, Out, OutStmt) of
                        {error, Error} ->
                            {error, Error, InStmts1, Log};
                        _ ->
                            {ok, InStmts1, Log}
                    end
            end
    end.
