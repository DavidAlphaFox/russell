-module(russell_def).

-export([file/1, find/4, match/3, apply/3]).

file(Filename) ->
    {ok, Bin} = file:read_file(Filename),
    case parse(binary_to_list(Bin)) of
        {ok, Def} ->
            {ok, maps:from_list(Def)};
        Error ->
            Error
    end.

parse(String) ->
    {ok, Tokens, _} = russell_lexer:string(String),
    russell_def_parser:parse(Tokens).

find(Name, NIn, NOut, Defs) ->
    case maps:find(Name, Defs) of
        error ->
            {error, {def_not_found, Name}};
        {ok, {In, Out} = Def} ->
            if length(In) =/= NIn ->
                    {error, {input_stmt_number_mismatch, length(In), NIn}};
               length(Out) =/= NOut ->
                    {error, {output_stmt_number_mismatch, length(Out), NOut}};
               true ->
                    {ok, Def}
            end
    end.


apply(InStmts, {In, Out}, C) ->
    case match(In, InStmts, #{}) of
        {error, _} = Error ->
            Error;
        Vars ->
            {Stmts, _, C1} = subst(Out, Vars, C),
            {ok, Stmts, C1}
    end.

match([], [], Vars) ->
    Vars;
match([H1|T1], [H2|T2], Vars) ->
    case match(H1, H2, Vars) of
        {error, _} = Error ->
            Error;
        Vars1 ->
            match(T1, T2, Vars1)
    end;
match({var, V}, X, Vars) ->
    case maps:find(V, Vars) of
        error ->
            Vars#{V => X};
        {ok, X} ->
            Vars;
        {ok, X1} ->
            {error, {not_match, X, X1}}
    end;
match(X, X, Vars) ->
    Vars;
match(X1, X2, _) ->
    {error, {not_match, X1, X2}}.


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
