-module(russell_prim_compile).

-export([run/1, format_error/1]).

run([Filename]) ->
    case russell_prim:parse(Filename) of
        {ok, Forms} ->
            Forms1 =
                [{attribute,0,file,{Filename,0}},
                 {attribute,0,module, list_to_atom(filename:basename(Filename, ".prim"))}
                 |transform_forms(Forms)],
            case compile:forms(Forms1, [verbose, return_errors, return_warnings, report_warnings]) of
                {ok,_,Bin,_} ->
                    io:format("~ts~n", [erl_prettypr:format(erl_syntax:form_list(Forms1))]),
                    ok = file:write_file([filename:rootname(Filename, ".prim"), ".beam"], Bin);
                {error, Errors, Warnings} ->
                    Errors1 = Errors ++ Warnings,
                    lists:foreach(
                      fun({F,E}) -> russell:file_error(F, {error, E}) end,
                      Errors1),
                    {error, Errors1}
            end;
        {error, _} = Error ->
            Error
    end.

format_error({def_not_found, Name}) ->
    io_lib:format("definition ~ts not found", [Name]).

transform_forms(Forms) ->
    {Forms1, _} =
        lists:mapfoldl(
          fun transform_form/2,
          #{},
          Forms),

    Forms2 =
        case [E || {error, _} = E <- Forms1] of
            [] ->
                [{function,0,main,1,
                 [{clause,0,[{var,0,'_'}],[],Forms1}]}];
            Errors ->
                Errors
        end,

    [{attribute,0,export,[{main,1}]}|Forms2].


transform_form({def, {Name, Line}, Ins, Out}, Defs) ->
    {Ins1, Subst, Next}= russell_core:subst(Ins, #{}, 0),
    {Out1, _, Next1} = russell_core:subst(Out, Subst, Next),

    {{match, Line, {atom, Line, undefined},
      {call, Line, {atom, Line, put},
       [{atom, Line, Name},
        {'fun', Line,
         {clauses,
          [{clause, Line,
            [transform_token(Tokens, Line) || Tokens <- Ins1],
            [],
            transform_vars(lists:seq(Next, Next1-1), Line)
            ++ [transform_token(Out1, Line)]
           }]
         }}
       ]}
     },
     Defs#{Name => {Ins1, Out1}}};
transform_form({proof, {{Name,Line}, In}, Body}, Defs) ->
    case maps:find(Name, Defs) of
        error ->
            {{error, {Line, ?MODULE, {def_not_found, Name}}}, Defs};
        {ok, {Ins, Out}} ->
            {In1, State} =
                lists:mapfoldl(
                  fun transform_def/2,
                  {#{}, 0},
                  In),
            {Body1, {_, Next}} =
                lists:mapfoldl(
                  fun transform_step/2,
                  State,
                  Body),

            {{call, Line,
              {'fun', Line,
               {clauses,
                [{clause, Line,
                  [{atom, Line, Name}],
                  [],
                  transform_vars(vars(Ins), Line)
                  ++ transform_inputs(lists:zip(In1, Ins))
                  ++ Body1
                  ++ [{match, Line, transform_token(Out, Line), transform_p({Next-1, Line})}]
                 }]
               }},
              [{atom, Line, Name}]
             },
             Defs}
    end.

transform_def({Name, Line}, {Def, Next}) ->
    {{Next, Line}, {Def#{Name=>Next}, Next+1}}.

transform_step({{Name, Line}, [Out|Ins]}, {Def, Next}) ->
    Ins1 = [transform_use(In, Def) || In <- Ins],
    {Out1, State} = transform_def(Out, {Def, Next}),
    {{match, Line,
      transform_p(Out1),
      {call, Line,
       {call, Line, {atom, Line, get}, [{atom, Line, Name}]},
       [transform_p(I) || I <- Ins1]
      }
     }, State}.

transform_use({Name, Line}, Def) ->
    {maps:get(Name, Def), Line}.

transform_inputs(Inputs) ->
    [{match, Line,
      transform_p(Name),
      transform_token(Tokens, Line)}
     || {{_, Line}=Name, Tokens} <- Inputs].

transform_p({P, Line}) ->
    {var, Line, list_to_atom([$P|integer_to_list(P)])}.

transform_vars(Vars, Line) ->
    [ {match,Line,
       transform_token({var,I}, Line),
       {call,Line,{atom,Line,make_ref},[]}}
      || I <- Vars].

transform_token([], Line) ->
    {nil, Line};
transform_token([H|T], Line) ->
    {cons, Line, transform_token(H, Line), transform_token(T, Line)};
transform_token({var, V}, Line) when is_integer(V) ->
    {var, Line, list_to_atom([$V|integer_to_list(V)])};
transform_token(A, Line) when is_atom(A) ->
    {atom, Line, A}.

vars(P) ->
    sets:to_list(vars(P, sets:new())).

vars([], Acc) ->
    Acc;
vars([H|T], Acc) ->
    vars(T, vars(H, Acc));
vars({var, X}, Acc) ->
    sets:add_element(X, Acc);
vars(_, Acc) ->
    Acc.
