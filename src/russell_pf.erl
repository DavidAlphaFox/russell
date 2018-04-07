-module(russell_pf).

-export([file/1, format_error/1, verify/2]).

file(Filename) ->
    {ok, Bin} = file:read_file(Filename),
    case parse(binary_to_list(Bin)) of
        {ok, Proof} ->
            case validate(Proof) of
                ok ->
                    {ok, transform(Proof)};
                Error ->
                    Error
            end;
        Error ->
            Error
    end.

parse(String) ->
    {ok, Tokens, _} = russell_lexer:string(String),
    russell_pf_parser:parse(Tokens).


format_error({stmt_defined, S}) ->
    io_lib:format("statement ~w already defined.", [S]);
format_error({stmt_not_found, S}) ->
    io_lib:format("statement ~w not found.", [S]).

validate({{_, In, Out}, Body}) ->
    case validate_defs(In, sets:new()) of
        {error, _} = Error ->
            Error;
        {ok, Def} ->
            case validate_stmts(Body, Def) of
                {error, _} = Error ->
                    Error;
                {ok, Def1} ->
                    case validate_uses(Out, Def1) of
                        {error, _} = Error ->
                            Error;
                        {ok, _} ->
                            ok
                    end
            end
    end.

validate_stmts([], Def) ->
    {ok, Def};
validate_stmts([{_, In, Out}|T], Def) ->
    case validate_uses(In, Def) of
        {error, _} = Error ->
            Error;
        {ok, Def1} ->
            case validate_defs(Out, Def1) of
                {error, _} = Error ->
                    Error;
                {ok, Def2} ->
                    validate_stmts(T, Def2)
            end
    end.

validate_defs([], Def) ->
    {ok, Def};
validate_defs([{S, Line}|T], Def) ->
    case sets:is_element(S, Def) of
        true ->
            {error, {Line, ?MODULE, {stmt_defined, S}}};
        false ->
            validate_defs(T, sets:add_element(S, Def))
    end.

validate_uses([], Def) ->
    {ok, Def};
validate_uses([{S, Line}|T], Def) ->
    case sets:is_element(S, Def) of
        false ->
            {error, {Line, ?MODULE, {stmt_not_found, S}}};
        true ->
            validate_uses(T, Def)
    end.

transform({Head, Body}) ->
    {transform_stmt(Head),
     [transform_stmt(S) || S <- Body]}.

transform_stmt({Name, Ins, Outs}) ->
    {Name, transform_symbols(Ins), transform_symbols(Outs)}.

transform_symbols(Symbols) ->
    [S || {S, _} <- Symbols].


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
