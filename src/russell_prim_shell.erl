-module(russell_prim_shell).

-export([run/1, format_error/1]).

format_error({unknown_command, Name}) ->
    io_lib:format("unknown command ~ts", [Name]);
format_error(step_not_allowed) ->
    "proof step not allowed".


run([Filename]) ->
    case russell_prim:parse(Filename) of
        {ok, Forms} ->
            case verify_forms(Forms, #{}) of
                {ok, Defs} ->
                    server(Defs);
                {error, _} = Error ->
                    russell:file_error(Filename, Error)
            end;
        {error, _} = Error ->
            Error
    end.


verify_forms([], Defs) ->
    {ok, Defs};
verify_forms([{def, _, _, _}=H|T], Defs) ->
    case russell_prim:verify_form(H, Defs) of
        {ok, Defs1} ->
            verify_forms(T, Defs1);
        {error, _} = Error ->
            Error
    end;
verify_forms([_|T], Defs) ->
    verify_forms(T, Defs).


server(Defs) ->
    server_loop(none, Defs).


server_loop(State, Defs) ->
    case io:get_line(">>> ") of
        eof ->
            io:format("~n", []);
        Line ->
            {ok, Tokens, _} = russell_prim_lexer:string(Line),
            case handle_tokens(Tokens, State, Defs) of
                {error, {_, M, E}} ->
                    io:format("ERROR: ~ts~n", [M:format_error(E)]),
                    State1 = State;
                {ok, State1} ->
                    ok
            end,
            server_loop(State1, Defs)
    end.

handle_tokens([], State, _) ->
    {ok, State};
handle_tokens(Tokens, State, Defs) ->
    case russell_prim_shell_parser:parse(Tokens) of
        {error, _} = Error ->
            Error;
        {ok, {{var, _, V}, Args}} ->
            eval_cmd(V, Args, State, Defs);
        {ok, {{atom, Line, A}, Args}} ->
            eval_step({A,Line}, Args, State, Defs)
    end.

eval_cmd(quit, [], _, _) ->
    halt();
eval_cmd(def, [Name], State, Defs) ->
    case russell_core:find(Name, Defs) of
        {error, _} = Error ->
            Error;
        {ok, {In, Out}} ->
            io:format(
              "~ts. ~ts.~n",
              [[["  ",russell_core:format_tokens(I), ".\n"]
                || I <- In],
               russell_core:format_tokens(Out)]),
            {ok, State}
    end;
eval_cmd(prove, [Name], _, Defs) ->
    case russell_core:find(Name, Defs) of
        {error, _} = Error ->
            Error;
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
eval_cmd(qed, [], {prove, State = #{input := Ins, steps := Steps, goal := Goal}}, _) ->
    {_, Out} = lists:last(Steps),
    case russell_core:verify_output(Ins, Out, Goal) of
        {error, _} = Error ->
            Error;
        _ ->
            show_all(State),
            {ok, {done, State}}
    end;
eval_cmd(save, [], {done, State = #{name := Name, steps := Steps, input := Ins}}, _) ->
    io:format(
      "~ts",
      [russell_prim:format([{proof, {Name, [I || {I,_} <-Ins]}, [{N,[O|I]} || {{N,I},{O,_}} <- Steps]}])]),
    {ok, State};
eval_cmd(dump, [], {Type, State}, _) ->
    show_all(State),
    {ok, {Type, State}};
eval_cmd(Name, _, _, _) ->
    {error, {1, ?MODULE, {unknown_command, Name}}}.

eval_step(Name, Ins, {prove, State = #{stmts := Stmts, counter := Counter}}, Defs) ->
    Defined = sets:from_list(maps:keys(Stmts)),

    case russell_prim:validate_uses(Ins, Defined) of
        {error, _} = Error ->
            Error;
        {ok, _} ->
            case russell_core:verify_step(Name, Ins, Stmts, Counter, Defs) of
                {error, _} = Error ->
                    Error;
                {ok, OutStmt, Counter1} ->
                    {Out, State1 = #{steps := Steps}} = add_stmt(OutStmt, State#{counter => Counter1}),
                    Step = {{Name,Ins},{{Out,1},OutStmt}},
                    {ok, {prove, State1#{steps := Steps ++ [Step]}}}
            end
    end;
eval_step(_, _, _, _) ->
    {error, {1, ?MODULE, step_not_allowed}}.

show_all(#{steps := Steps, input := Inputs}) ->
    io:format(
      "~ts~n~ts~n",
      [russell_prim:format_stmts(Inputs),
       russell_prim:format_steps(Steps)]).

add_stmts(Stmts, State) ->
    lists:mapfoldl(fun add_stmt/2, State, Stmts).

add_stmt(Stmt, State = #{next := Next, stmts := Stmts}) ->
    Name = list_to_atom(integer_to_list(Next)),
    io:format("~ts~n", [russell_prim:format_stmt({{Name, 1}, Stmt})]),
    {Name, State#{next => Next + 1, stmts => Stmts#{Name => Stmt}}}.
