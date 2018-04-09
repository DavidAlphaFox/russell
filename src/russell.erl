-module(russell).

-export([main/1, run/1]).

main(Args) ->
    case run(Args) of
        ok ->
            halt(0);
        _ ->
            halt(1)
    end.

run([Filename|Args]) ->
    case russell_def:file(Filename) of
        {ok, Defs} ->
            run(Defs, Args);
        {error, {L, M, E}} ->
            io:format("~s:~B: ~s~n", [Filename, L, M:format_error(E)]),
            error
    end.

run(Defs, []) ->
    russell_shell:server(Defs),
    ok;
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
            io:format("~s:~B: ~s~n", [Filename, L, M:format_error(E)]),
            error;
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
               russell_pf:format_steps(Steps)]),
            {error, Error};
        {ok, Ins, Steps} ->
            io:format(
              "~s~n~s~n",
              [russell_def:format_stmts(Ins),
               russell_pf:format_steps(Steps)]),
            ok
    end.
