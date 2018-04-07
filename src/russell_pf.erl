-module(russell_pf).

-export([file/1, verify/2]).

file(Filename) ->
    {ok, Bin} = file:read_file(Filename),
    parse(binary_to_list(Bin)).


parse(String) ->
    {ok, Tokens, _} = russell_lexer:string(String),
    russell_pf_parser:parse(Tokens).


verify(Defs, {{Name, InNames, OutNames}, Steps}) ->
    case russell_def:find(Name, length(InNames), length(OutNames), Defs) of
        {error, _} = Error ->
            Error;
        {ok, {InStmts, OutStmts}} ->
            Ins = lists:zip(InNames, InStmts),
            case verify_steps(Steps, maps:from_list(Ins), Defs) of
                {error, Error, Log} ->
                    {error, Error, Ins, Log};
                {ok, Stmts, Log} ->
                    OutStmts1 = [ maps:get(N,Stmts) || N <- OutNames],

                    case russell_def:match(InStmts ++ OutStmts, InStmts ++ OutStmts1, #{}) of
                        {error, Error} ->
                            {error, Error, Ins, Log};
                        _ ->
                            {ok, Ins, Log}
                    end
            end
    end.


verify_steps([], Stmts, _) ->
    {ok, Stmts, []};
verify_steps([{Name, In, _} = H|T], Stmts, Defs) ->
    case verify_step(H, Stmts, Defs) of
        {error, Error} ->
            {error, {{Name, In}, Error}, []};
        {ok, Stmts1, Step} ->
            {Tag, Value, Steps} = verify_steps(T, Stmts1, Defs),
            {Tag, Value, [{{Name, In}, Step}|Steps]}
    end.


verify_step({Name, InNames, OutNames}, Stmts, Defs) ->
    case russell_def:find(Name, length(InNames), length(OutNames), Defs) of
        {error, _} = Error ->
            Error;
        {ok, Def} ->
            InStmts = [ maps:get(N,Stmts) || N <- InNames],
            case russell_def:apply(InStmts, Def) of
                {error, _} = Error ->
                    Error;
                {ok, OutStmts} ->
                    Outs =
                        [ {N,V}
                          || {N,V} <- lists:zip(OutNames, OutStmts),
                             N =/= '_' ],
                    Stmts1 = maps:merge(Stmts, maps:from_list(Outs)),
                    {ok, Stmts1, Outs}
            end
    end.
