-module(russell).

-export([main/1, file_error/2]).

main(Args) ->
    case catch run(Args) of
        ok ->
            halt(0);
        Other ->
            io:format("~p~n", [Other]),
            halt(1)
    end.

run(["pf",Arg]) ->
    russell_pf_shell:run([Arg]);
run(["pf"|Args]) ->
    russell_verify:run(Args);
run(["pfs"|Args]) ->
    russell_pfs:run(Args).

file_error(Filename, {error, {L,M,E}}=Error) ->
    io:format("~s:~B: ~s~n", [Filename, L, M:format_error(E)]),
    Error;
file_error(_, Other) ->
    Other.
