-module(russell_shell).

-export([server/1, format_error/1]).

format_error({unknown_command, Name}) ->
    io_lib:format("unknown command ~s", [Name]);
format_error(step_not_allowed) ->
    "proof step not allowed".

server(Defs) ->
    server_loop(none, Defs).

server_loop(State, Defs) ->
    case io:get_line(">>> ") of
        eof ->
            io:format("~n", []);
        Line ->
            {ok, Tokens, _} = russell_lexer:string(Line),
            case handle_tokens(Tokens, State, Defs) of
                {error, {_,M,E}} ->
                    io:format("ERROR: ~s~n", [M:format_error(E)]),
                    State1 = State;
                {ok, State1} ->
                    ok
            end,
            server_loop(State1, Defs)
    end.

handle_tokens([], State, _) ->
    {ok, State};
handle_tokens(Tokens, State, Defs) ->
    case russell_shell_parser:parse(Tokens) of
        {error, _} = Error ->
            Error;
        {ok, {cmd, Cmd}} ->
            eval_cmd(Cmd, State, Defs);
        {ok, {step, Step}} ->
            eval_step(Step, State, Defs)
    end.

eval_cmd({{'.quit', _}, _}, _, _) ->
    halt();
eval_cmd({{'.prove', _}, [{Name, _}]}, State, Defs) ->
    case maps:find(Name, Defs) of
        error ->
            io:format("ERROR: definition not found ~s~n", [atom_to_list(Name)]),
            {ok, State};
        {ok, {In, Out}} ->
            State1 =
                #{name => Name,
                  next => 1,
                  counter => 0,
                  steps => [],
                  stmts => #{},
                  goal => Out},
            {In1, State2} = add_stmts(In, State1),
            {ok, {prove, State2#{input => [{I,1} || I <- In1]}}}
    end;
eval_cmd({{'.qed', Line}, Outs}, {prove, #{goal := Goal}}, _) when length(Goal) =/= length(Outs) ->
    {error, {Line, russell_pf, {output_number_mismatch, length(Goal), length(Outs)}}};
eval_cmd({{'.qed', _}, Outs}, {prove, State = #{input := Ins, stmts := Stmts, goal := Goal}}, _) ->
    Defined = sets:from_list(maps:keys(Stmts)),
    case russell_pf:validate_uses(Ins, Defined) of
        {error, _} = Error ->
            Error;
        {ok, _} ->
            case russell_pf:verify_output(Ins, Outs, Goal, Stmts) of
                {error, _} = Error ->
                    Error;
                _ ->
                    show_all({prove, State}),
                    {ok, {done, State#{output => Outs}}}
            end
    end;
eval_cmd({{'.save',_}, [{Filename,_}]}, State = {done, #{name := Name, steps := Steps, input := Ins, output := Outs}}, _) ->
    Bin = russell_pf:format({{{Name, 1}, Ins, Outs}, Steps}),
    case file:write_file(Filename, Bin) of
        ok ->
            ok;
        _ ->
            io:format("ERROR: failed to write file ~s~n", [Filename])
    end,
    {ok, State};
eval_cmd({{'.dump', _}, []}, State, _) ->
    show_all(State),
    {ok, State};
eval_cmd({{Name, Line}, _}, _, _) ->
    {error, {Line, ?MODULE, {unknown_command, Name}}}.

show_all({_, #{steps := Steps, stmts := Stmts, input := Inputs}}) ->
    io:format(
      "~s~n~s~n",
      [russell_def:format_stmts([{I, maps:get(I, Stmts)} || {I,_} <- Inputs]),
       russell:format_steps([{{N,I},[{O, maps:get(O,Stmts)} || {O,_} <- Out]} || {N,I,Out} <- Steps])]).

add_stmts(Stmts, State) ->
    lists:mapfoldl(fun add_stmt/2, State, Stmts).

add_stmt(Stmt, State = #{next := Next, stmts := Stmts}) ->
    Name = list_to_atom(integer_to_list(Next)),
    io:format("~s~n", [russell_def:format_stmt({Name, Stmt})]),
    {Name, State#{next => Next + 1, stmts => Stmts#{Name => Stmt}}}.

eval_step({Name, Ins}, {prove, State = #{stmts := Stmts, counter := Counter}}, Defs) ->
    Defined = sets:from_list(maps:keys(Stmts)),

    case russell_pf:validate_uses(Ins, Defined) of
        {error, _} = Error ->
            Error;
        {ok, _} ->
            case russell_pf:verify_step(Name, Ins, Stmts, Counter, Defs) of
                {error, _} = Error ->
                    Error;
                {ok, OutStmts, Counter1} ->
                    {Outs, State1 = #{steps := Steps}} = add_stmts(OutStmts, State#{counter => Counter1}),
                    Step = {Name,Ins,[{O,1} || O <- Outs]},
                    {ok, {prove, State1#{steps := Steps ++ [Step]}}}
            end
    end;
eval_step({{_, Line}, _}, _, _) ->
    {error, {Line, ?MODULE, step_not_allowed}}.
