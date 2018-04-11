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

file_error(Filename, {error, [_|_]=Errors} = Error) ->
    lists:foreach(
      fun(E) -> print_error(Filename, E) end,
      Errors),
    Error;
file_error(Filename, {error, E}=Error) ->
    print_error(Filename, E),
    Error;
file_error(_, Other) ->
    Other.

print_error(Filename, {L,M,E}) ->
    io:format("~s:~B: ~s~n", [Filename, L, M:format_error(E)]);
print_error(Filename, E) ->
    io:format("~s: ERROR ~p~n", [Filename, E]).
