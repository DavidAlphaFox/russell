-module(russell_prim).

-export(
   [run/1, format_error/1,
    parse/1]).

run([Filename]) ->
    case parse(Filename) of
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

format_error({file_not_exist, Filename}) ->
    io_lib:format("~ts: file not exist", [Filename]);
format_error({duplicate_def, Name}) ->
    io_lib:format("duplicate definition of ~ts", [Name]);
format_error({stmt_defined, S}) ->
    io_lib:format("statement ~w already defined.", [S]);
format_error({stmt_not_found, S}) ->
    io_lib:format("statement ~w not found.", [S]).

parse(Filename) ->
    case file:read_file(Filename) of
        {error, enoent} ->
            russell:file_error(?MODULE, {error, {?LINE, ?MODULE, {file_not_exist, Filename}}});
        {ok, Bin} ->
            case parse_string(binary_to_list(Bin)) of
                {ok, Forms} ->
                    case validate_forms(Forms) of
                        [] ->
                            {ok, Forms};
                        Errors ->
                            russell:file_error(Filename, {error, Errors})
                    end;
                Error ->
                    russell:file_error(Filename, Error)
            end
    end.

parse_string(String) ->
    case russell_prim_lexer:string(String) of
        {ok, Tokens, _} ->
            russell_prim_parser:parse(Tokens);
        Error ->
            Error
    end.

validate_forms([]) ->
    [];
validate_forms([H|T]) ->
    case validate_form(H) of
        ok ->
            validate_forms(T);
        {error, Error} ->
            [Error|validate_forms(T)]
    end.

validate_form({def, _, _, _}) ->
    ok;
validate_form({proof, {_, In}, Body}) ->
    case fold(
           fun({Fun, List}, Acc) ->
                   fold(Fun, Acc, List)
           end,
           sets:new(),
           [{fun validate_def/2, In}, {fun validate_stmt/2, Body}])
    of
        {error, _} = Error ->
            Error;
        {ok, _} ->
            ok
    end.

validate_stmt({_, [Out|In]}, Def) ->
    case fold(fun validate_use/2, Def, In) of
        {error, _} = Error ->
            Error;
        {ok, Def1} ->
            validate_def(Out, Def1)
    end.

validate_def({S, Line}, Def) ->
    case sets:is_element(S, Def) of
        true ->
            {error, {Line, ?MODULE, {stmt_defined, S}}};
        false ->
            {ok, sets:add_element(S, Def)}
    end.

validate_use({S, Line}, Def) ->
    case sets:is_element(S, Def) of
        false ->
            {error, {Line, ?MODULE, {stmt_not_found, S}}};
        true ->
            {ok, Def}
    end.


verify_forms([], _) ->
    [];
verify_forms([H|T], Defs) ->
    case verify_form(H, Defs) of
        {ok, Defs1} ->
            verify_forms(T, Defs1);
        {error, Error} ->
            [Error|verify_forms(T, Defs)]
    end.


verify_form({def, {Name, Line}, Ins, Out}, Defs) ->
    case maps:is_key(Name, Defs) of
        true ->
            {error, {Line, ?MODULE, {duplicate_def, Name}}};
        false ->
            {ok, Defs#{Name => {Ins, Out}}}
    end;
verify_form({proof, {{N,_}=Name, In}, Body}, Defs) ->
    case russell_verify:verify_proof({Name, In, Body}, Defs) of
        {error, _} = Error ->
            Error;
        {error, Error, Ins, Steps} ->
            io:format(
              "~ts~n~ts~n~ts~n",
              [N,
               format_stmts(Ins),
               format_steps(Steps)]),
            {error, Error};
        {ok, Ins, Steps} ->
            io:format(
              "~ts~n~ts~n~ts~n",
              [N,
               format_stmts(Ins),
               format_steps(Steps)]),
            {ok, Defs}
    end.

fold(_, Acc, []) ->
    {ok, Acc};
fold(Fun, Acc, [H|T]) ->
    case Fun(H, Acc) of
        {error, _} = Error ->
            Error;
        {ok, Acc1} ->
            fold(Fun, Acc1, T)
    end.


format_stmts(Stmts) ->
    string:join([["  ", format_stmt(S)] || S <- Stmts], "\n").

format_stmt({{Name, _}, Tokens}) ->
    io_lib:format(
      "(~ts) ~ts",
      [Name, russell_verify:format_tokens(Tokens)]).


format_steps(Steps) ->
    string:join([format_step(S) || S <- Steps], "\n").

format_step({{{Name,_}, Ins}, Stmt}) ->
    io_lib:format(
      " ~ts~ts:~n  ~ts",
      [Name,
       [io_lib:format(" ~ts",[A]) || {A,_} <- Ins],
       format_stmt(Stmt)]).
