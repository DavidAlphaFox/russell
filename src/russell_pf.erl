-module(russell_pf).

-export([file/1, format/1, format_error/1, validate_uses/2, verify/2, verify_output/4, verify_step/5]).

file(Filename) ->
    {ok, Bin} = file:read_file(Filename),
    case parse(binary_to_list(Bin)) of
        {ok, Proof} ->
            case validate(Proof) of
                ok ->
                    {ok, Proof};
                Error ->
                    Error
            end;
        Error ->
            Error
    end.

parse(String) ->
    {ok, Tokens, _} = russell_lexer:string(String),
    russell_pf_parser:parse(Tokens).


format({Head, Body}) ->
    [format_stmt(Head),
     "\n: ",
     string:join([[format_stmt(S), ",\n"] || S <- Body], "  ")].

format_stmt({{Name,_}, Ins, Outs}) ->
    io_lib:format(
      "~s : ~s ~s",
      [format_names(Outs), Name, format_names(Ins)]).

format_names(Names) ->
    string:join([io_lib:format("~s", [Name]) || {Name,_} <- Names], " ").

format_error({stmt_defined, S}) ->
    io_lib:format("statement ~w already defined.", [S]);
format_error({stmt_not_found, S}) ->
    io_lib:format("statement ~w not found.", [S]);
format_error({output_number_mismatch, E, G}) ->
    io_lib:format("output number mismatch, expected ~B, got ~B", [E,G]).

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


verify(Defs, {{Name={_,Line}, InNames, OutNames}, Steps}) ->
    case russell_def:find(Name, length(InNames), Defs) of
        {error, _} = Error ->
            Error;
        {ok, {_, OutStmts}} when length(OutStmts) =/= length(OutNames) ->
            {error, {Line, ?MODULE, {output_number_mismatch, length(OutStmts), length(OutNames)}}};
        {ok, {InStmts, OutStmts}} ->
            InStmts1 = lists:zip([N || {N,_} <- InNames], InStmts),
            case verify_steps(Steps, maps:from_list(InStmts1), 0, Defs) of
                {error, Error, Log} ->
                    {error, Error, InStmts1, Log};
                {ok, Stmts, _, Log} ->
                    case verify_output(InNames, OutNames, OutStmts, Stmts) of
                        {error, Error} ->
                            {error, Error, InStmts1, Log};
                        _ ->
                            {ok, InStmts1, Log}
                    end
            end
    end.

verify_output(InNames, OutNames, OutStmts, Stmts) ->
    Ins = [{{N,L}, maps:get(N, Stmts)} || {N,L} <- InNames],
    InStmts = [I || {_,I} <- Ins],
    Outs = [{{N,L}, maps:get(N, Stmts)} || {N,L} <- OutNames],
    russell_def:match_stmts(Ins ++ Outs, InStmts ++ OutStmts, #{}).

verify_steps([], Stmts, Counter, _) ->
    {ok, Stmts, Counter, []};
verify_steps([{Name, In, _} = H|T], Stmts, Counter, Defs) ->
    case verify_step(H, Stmts, Counter, Defs) of
        {error, Error} ->
            {error, Error, []};
        {ok, Stmts1, Counter1, Outs} ->
            Step = {{Name, In}, Outs},
            case verify_steps(T, Stmts1, Counter1, Defs) of
                {error, Error, Steps} ->
                    {error, Error, [Step|Steps]};
                {ok, Stmts2, Counter2, Steps} ->
                    {ok, Stmts2, Counter2, [Step|Steps]}
            end
    end.


verify_step({Name={_,Line}, InNames, OutNames}, Stmts, Counter, Defs) ->
    case verify_step(Name, InNames, Stmts, Counter, Defs) of
        {error, _} = Error ->
            Error;
        {ok, OutStmts, _} when length(OutStmts) =/= length(OutNames) ->
            {error, {Line, ?MODULE, {output_number_mismatch, length(OutStmts), length(OutNames)}}};
        {ok, OutStmts, Counter1} ->
            Outs =
                [ {N,V}
                  || {{N,_},V} <- lists:zip(OutNames, OutStmts),
                     N =/= '_' ],
            Stmts1 = maps:merge(Stmts, maps:from_list(Outs)),
            {ok, Stmts1, Counter1, Outs}
    end.


verify_step(Name, InNames, Stmts, Counter, Defs) ->
    case russell_def:find(Name, length(InNames), Defs) of
        {error, _} = Error ->
            Error;
        {ok, Def} ->
            InStmts =
                [ {{N,L}, maps:get(N,Stmts)}
                  || {N,L} <- InNames],
            russell_def:apply(InStmts, Def, Counter)
    end.
