-module(russell_verify).

-export([run/1]).

run([DFN, PFN]) ->
    {ok, Defs} = russell:file_error(DFN, russell_def:file(DFN)),
    {ok, Proof} = russell:file_error(PFN, russell_pf:file(PFN)),
    russell:file_error(PFN, verify(maps:from_list(Defs), Proof)).

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
