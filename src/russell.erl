-module(russell).

-export([main/1, format_steps/1]).

main([Filename|Args]) ->
    case russell_def:file(Filename) of
        {ok, Defs} ->
            run(Defs, Args);
        {error, {L, M, E}} ->
            io:format("~s:~B: ~s~n", [Filename, L, M:format_error(E)])
    end.

run(Defs, []) ->
    russell_shell:server(Defs);
run(Defs, [Filename]) ->
    case
        case russell_pf:file(Filename) of
            {ok, Proof} ->
                verify(Defs, Proof);
            {error, _} = Error ->
                Error
        end
    of
        {error, {L, M, E}} ->
            io:format("~s:~B: ~s~n", [Filename, L, M:format_error(E)]);
        _ ->
            ok
    end.

verify(Defs, Proof) ->
    case russell_pf:verify(Defs, Proof) of
        {error, _} = Error ->
            Error;
        {error, Error, Ins, Steps} ->
            io:format(
              "~s~n~s~n",
              [russell_def:format_stmts(Ins),
               format_steps(Steps)]),
            {error, Error};
        {ok, Ins, Steps} ->
            io:format(
              "~s~n~s~n",
              [russell_def:format_stmts(Ins),
               format_steps(Steps)]),
            ok
    end.

format_steps(Steps) ->
    string:join([format_step(S) || S <- Steps], "\n").

format_step({{{Name,_}, Ins}, Stmts}) ->
    [io_lib:format("~s ~s:~n", [Name, string:join([atom_to_list(A) || {A,_} <- Ins], " ")]),
     russell_def:format_stmts(Stmts)].
