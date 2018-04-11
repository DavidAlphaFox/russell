-module(russell_pf_shell).

-export([run/1, format_error/1]).

format_error({unknown_command, Name}) ->
    io_lib:format("unknown command ~s", [Name]);
format_error(step_not_allowed) ->
    "proof step not allowed".

run([DFN]) ->
    {ok, Defs} = russell:file_error(DFN, russell_def:file(DFN)),
    server(maps:from_list(Defs)).

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
    case russell_pf_shell_parser:parse(Tokens) of
        {error, _} = Error ->
            Error;
        {ok, {cmd, Cmd}} ->
            eval_cmd(Cmd, State, Defs);
        {ok, {step, Step}} ->
            eval_step(Step, State, Defs)
    end.

eval_cmd({{'.quit', _}, _}, _, _) ->
    halt();
eval_cmd({{'.def', _}, [Name]}, State, Def) ->
    case russell_def:find(Name, Def) of
        {error, _} = Error ->
            Error;
        {ok, {In, Out}} ->
            io:format(
              "~s: ~s.~n",
              [[["  ",russell_def:format_tokens(I), ",\n"] || I <- In],
               russell_def:format_tokens(Out)]),
            {ok, State}
    end;
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
            {ok, {prove, State2#{input => lists:zip([{I,1} || I <- In1], In)}}}
    end;
eval_cmd({{'.qed', _}, []}, {prove, State = #{input := Ins, steps := Steps, goal := Goal}}, _) ->
    {_, Out} = lists:last(Steps),
    case russell_pf:verify_output(Ins, Out, Goal) of
        {error, _} = Error ->
            Error;
        _ ->
            show_all({prove, State}),
            {ok, {done, State}}
    end;
eval_cmd({{'.save',_}, [{Filename,_}]}, State = {done, #{name := Name, steps := Steps, input := Ins}}, _) ->
    Bin = russell_pf:format({{Name, 1}, [I || {I,_} <-Ins], [{N,I,O} || {{N,I},{O,_}} <- Steps]}),
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

show_all({_, #{steps := Steps, input := Inputs}}) ->
    io:format(
      "~s~n~s~n",
      [russell_def:format_stmts(Inputs),
       russell_pf:format_steps(Steps)]).

add_stmts(Stmts, State) ->
    lists:mapfoldl(fun add_stmt/2, State, Stmts).

add_stmt(Stmt, State = #{next := Next, stmts := Stmts}) ->
    Name = list_to_atom(integer_to_list(Next)),
    io:format("~s~n", [russell_def:format_stmt({{Name, 1}, Stmt})]),
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
                {ok, OutStmt, Counter1} ->
                    {Out, State1 = #{steps := Steps}} = add_stmt(OutStmt, State#{counter => Counter1}),
                    Step = {{Name,Ins},{{Out,1},OutStmt}},
                    {ok, {prove, State1#{steps := Steps ++ [Step]}}}
            end
    end;
eval_step({{_, Line}, _}, _, _) ->
    {error, {Line, ?MODULE, step_not_allowed}}.
