-module(russell_prim_verify).

-export([run/1]).

run([Filename]) ->
    case russell_prim:parse(Filename) of
        {ok, Forms} ->
            case verify_forms(Forms, #{}) of
                [] ->
                    ok;
                Errors ->
                    russell:file_error(Filename, {error, Errors})
            end;
        {error, _} = Error ->
            Error
    end.

verify_forms([], _) ->
    [];
verify_forms([H|T], Defs) ->
    case russell_prim:verify_form(H, Defs) of
        {ok, Defs1} ->
            verify_forms(T, Defs1);
        {error, Error} ->
            [Error|verify_forms(T, Defs)]
    end.
