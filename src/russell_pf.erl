-module(russell_pf).

-export([file/1, format_steps/1, format/1, format_error/1, validate_uses/2, verify/2, verify_output/3, verify_step/5]).

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


format_steps(Steps) ->
    string:join([format_step(S) || S <- Steps], "\n").

format_step({{{Name,_}, Ins}, Stmt}) ->
    io_lib:format(
      "~s~s:~n  ~s",
      [Name,
       [io_lib:format(" ~s",[A]) || {A,_} <- Ins],
       russell_def:format_stmt(Stmt)]).


format({Name, In, Body}) ->
    [format_stmt({Name, In}),
     ":\n",
     string:join([format_stmt(S) || S <- Body], ",\n"),
     ".\n"].

format_stmt({{Name, _}, In}) ->
    io_lib:format(
      "~s~s",
      [Name, format_names(In)]);
format_stmt({Name, In, Out}) ->
    [ format_names([Out]),
      " ",
      format_stmt({Name, In})].

format_names(Names) ->
    [io_lib:format(" ~s", [Name]) || {Name,_} <- Names].

format_error({stmt_defined, S}) ->
    io_lib:format("statement ~w already defined.", [S]);
format_error({stmt_not_found, S}) ->
    io_lib:format("statement ~w not found.", [S]).

validate({_, In, Body}) ->
    case validate_defs(In, sets:new()) of
        {error, _} = Error ->
            Error;
        {ok, Def} ->
            case validate_stmts(Body, Def) of
                {error, _} = Error ->
                    Error;
                {ok, _} ->
                    ok
            end
    end.

validate_stmts([], Def) ->
    {ok, Def};
validate_stmts([{_, In, Out}|T], Def) ->
    case validate_uses(In, Def) of
        {error, _} = Error ->
            Error;
        {ok, Def1} ->
            case validate_defs([Out], Def1) of
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


verify(Defs, {Name, InNames, Steps}) ->
    case russell_def:find_exact(Name, length(InNames), Defs) of
        {error, _} = Error ->
            Error;
        {ok, {InStmts, OutStmt}} ->
            InStmts1 = lists:zip(InNames, InStmts),
            case verify_steps(Steps, maps:from_list([{N,S} || {{N,_},S} <- InStmts1]), 0, Defs) of
                {error, Error, Log} ->
                    {error, Error, InStmts1, Log};
                {ok, Log} ->
                    {_, Out} = lists:last(Log),
                    case verify_output(InStmts1, Out, OutStmt) of
                        {error, Error} ->
                            {error, Error, InStmts1, Log};
                        _ ->
                            {ok, InStmts1, Log}
                    end
            end
    end.

verify_output(Ins, Out, OutStmt) ->
    InStmts = [I || {_,I} <- Ins],
    russell_def:match_stmts(Ins ++ [Out], InStmts ++ [OutStmt], #{}).

verify_steps([], _, _, _) ->
    {ok, []};
verify_steps([{Name, InNames, {OutName, _}=Out}|T], Stmts, Counter, Defs) ->
    case verify_step(Name, InNames, Stmts, Counter, Defs) of
        {error, Error} ->
            {error, Error, []};
        {ok, OutStmt, Counter1} ->
            Step = {{Name, InNames}, {Out, OutStmt}},
            case verify_steps(T, Stmts#{OutName => OutStmt}, Counter1, Defs) of
                {error, Error, Steps} ->
                    {error, Error, [Step|Steps]};
                {ok, Steps} ->
                    {ok, [Step|Steps]}
            end
    end.

verify_step(Name, InNames, Stmts, Counter, Defs) ->
    case russell_def:find_exact(Name, length(InNames), Defs) of
        {error, _} = Error ->
            Error;
        {ok, Def} ->
            InStmts =
                [ {{N,L}, maps:get(N,Stmts)}
                  || {N,L} <- InNames],
            russell_def:apply(InStmts, Def, Counter)
    end.
