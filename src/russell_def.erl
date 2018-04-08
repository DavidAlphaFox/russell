-module(russell_def).

-export([file/1, format_stmts/1, format_stmt/1, format_error/1, find/3, match_stmts/3, apply/3]).

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

format_stmts(Stmts) ->
    string:join([["  ", format_stmt(S)] || S <- Stmts], "~n").

format_stmt({Name, Tokens}) ->
    io_lib:format("(~s) ~s", [Name, format_tokens(Tokens)]).

format_tokens(Tokens) ->
    string:join([format_token(T) || T <- Tokens], " ").

format_token(List) when is_list(List) ->
    ["(", format_tokens(List), ")"];
format_token({var, V}) when is_atom(V) ->
    atom_to_list(V);
format_token({var, V}) when is_integer(V) ->
    io_lib:format(".~B", [V]);
format_token(A) ->
    atom_to_list(A).

format_error({def_not_found, Name}) ->
    io_lib:format("definition ~s not found", [Name]);
format_error({input_number_mismatch, E, G}) ->
    io_lib:format("input number mismatch, expected ~B, got ~B", [E,G]);
format_error({not_match, E, G}) ->
    io_lib:format(
      "statement mismatch~n  Expected:~n    ~s~n  Got:~n    ~s",
      [format_tokens(E),format_tokens(G)]);
format_error({not_match, E, G, V, X}) ->
    io_lib:format(
      "statement mismatch~n  Expected:~n  ~s  Where ~s=~n    ~s~n  Got:~n    ~s",
      [format_tokens(E),V,format_token(X),format_tokens(G)]).


find({Name, Line}, NIn, Defs) ->
    case maps:find(Name, Defs) of
        error ->
            {error, {Line, ?MODULE, {def_not_found, Name}}};
        {ok, {In, _}} when length(In) =/= NIn ->
            {error, {Line, ?MODULE, {input_number_mismatch, length(In), NIn}}};
        {ok, Def} ->
            {ok, Def}
    end.


apply(InStmts, {In, Out}, C) ->
    case match_stmts(InStmts, In, #{}) of
        {error, _} = Error ->
            Error;
        {ok, Vars} ->
            {Stmts, _, C1} = subst(Out, Vars, C),
            {ok, Stmts, C1}
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

match_stmt({{_,Line}, Stmt}, In, Vars) ->
    case match(In, Stmt, Vars) of
        {error, not_match} ->
            {error, {Line,?MODULE,{not_match, In, Stmt}}};
        {error, {not_match, V, X}} ->
            {error, {Line,?MODULE,{not_match, In, Stmt, V, X}}};
        {ok, Vars1} ->
            {ok, Vars1}
    end.

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
