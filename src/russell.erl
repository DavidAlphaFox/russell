-module(russell).

-export([main/1]).

main([Filename|Args]) ->
    case russell_def:file(Filename) of
        {ok, Defs} ->
            run(Defs, Args);
        {error, {L, M, E}} ->
            io:format("~s:~B: ~s~n", [Filename, L, M:format_error(E)])
    end.

run(Defs, [Filename]) ->
    case russell_pf:file(Filename) of
        {ok, Proof} ->
            io:format("~s~n", [verify(Defs, Proof)]);
        {error, {L, M, E}} ->
            io:format("~s:~B: ~s~n", [Filename, L, M:format_error(E)])
    end.

verify(Defs, Proof) ->
    case russell_pf:verify(Defs, Proof) of
        {error, Error} ->
            format_error(Error);
        {error, Error, Ins, Steps} ->
            [format_stmts(Ins),
             format_steps(Steps),
             format_error(Error)];
        {ok, Ins, Steps} ->
            [format_stmts(Ins),
             format_steps(Steps)]
    end.

format_error(Error) ->
    io_lib:format("~p~n", [Error]).

format_steps(Steps) ->
    [format_step(S) || S <- Steps].

format_step({{Name, Ins}, Stmts}) ->
    [io_lib:format("~s ~s:~n", [atom_to_list(Name), format_tokens(Ins)]),
     format_stmts(Stmts)].

format_stmts(Stmts) ->
    [format_stmt(S) || S <- Stmts].

format_stmt({Name, Tokens}) ->
    io_lib:format("  (~s) ~s~n", [atom_to_list(Name), format_tokens(Tokens)]).

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
