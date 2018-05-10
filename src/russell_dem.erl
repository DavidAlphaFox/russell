-module(russell_dem).

-export([run/1, format_error/1]).

run([Filename|Args]) ->
    case parse(Filename) of
        {ok, Forms} ->
            case russell:file_error(Filename, verify_forms(Forms)) of
                {error, _} = Error ->
                    Error;
                {ok, Forms1} ->
                    case Args of
                        [] ->
                            io:format("~ts~n", [russell_prim:format(Forms1)]);
                        [Output] ->
                            ok = file:write_file(Output, russell_prim:format(Forms1))
                    end
            end;
        {error, _} = Error ->
            Error
    end.

format_error({def_not_found, Name}) ->
    io_lib:format("definition ~ts not found", [Name]);
format_error({num_not_found, Name}) ->
    io_lib:format("proposition numbered ~ts not found", [Name]);
format_error({file_not_exist, Filename}) ->
    io_lib:format("~ts: file not exist", [Filename]);
format_error({alias_not_found, Name}) ->
    io_lib:format("alias not found: ~ts", [Name]);
format_error({unbound_var_found, V}) ->
    io_lib:format("unbound variable found in substitution of ~ts", [V]);
format_error({subst_defined, V}) ->
    io_lib:format("substituion of ~ts already defined", [V]);
format_error(unification) ->
    "unification failed".

parse(Filename) ->
    case file:read_file(Filename) of
        {error, enoent} ->
            russell:file_error(?MODULE, {error, {?LINE, ?MODULE, {file_not_exist, Filename}}});
        {ok, Bin} ->
            russell:file_error(Filename, parse_string(binary_to_list(Bin)))
    end.


parse_string(String) ->
    case russell_dem_lexer:string(String) of
        {ok, Tokens, _} ->
            russell_dem_parser:parse(Tokens);
        {error, Error, _} ->
            {error, Error}
    end.

verify_forms(Forms) ->
    Prims =
        [{def, {'=', 1}, [[{var, p}, ':', prop]], [{var, p}, '=', {var, p}]},
         {def, {'|->', 1}, [['|-', {var, p}], [{var, p}, '=', {var, q}]], ['|-', {var, q}]},
         {def, {'|-<', 1}, [['|-', {var, p}], [{var, q}, '=', {var, p}]], ['|-', {var, q}]}],

    {ok, Defs} = verify_prim_forms(Prims, #{}),

    State=
        #{vars => #{},
          defs => Defs,
          df => [],
          alias => #{}},

    case verify_forms(Forms, State) of
        {ok, Forms1} ->
            {ok, Prims ++ Forms1};
        {error, _} = Error ->
            Error
    end.

verify_forms([], _) ->
    {ok, []};
verify_forms([H|T], State) ->
    case verify_form(H, State) of
        {ok, H1, State1 = #{defs := Defs}} ->
            case verify_prim_forms(H1, Defs) of
                {ok, Defs1} ->
                    case verify_forms(T, State1#{defs := Defs1}) of
                        {ok, T1} ->
                            {ok, H1++T1};
                        {error, _} = Error ->
                            Error
                    end;
                {error, _} = Error ->
                    Error
            end;
        {error, _} = Error ->
            Error
    end.


verify_prim_forms([], Defs) ->
    {ok, Defs};
verify_prim_forms([H|T], Defs) ->
    case russell_prim:verify_form(H, Defs) of
        {ok, Defs1} ->
            verify_prim_forms(T, Defs1);
        {error, _} = Error ->
            Error
    end.

verify_form({var, _, {symbol, _, Name}, Token}, State = #{vars := Vars}) ->
    {ok, [], State#{vars := Vars#{Name => Token}}};
verify_form({alias, _, {symbol, _, Name}, Names}, State = #{alias := Alias, defs := Defs}) ->
    case validate_alias(Names, Defs) of
        [] ->
            Names1 = [N || {name, _, {symbol, _, N}} <- Names],
            {ok, [], State#{alias := Alias#{Name => maps:get(Name, Alias, []) ++ Names1}}};
        Errors ->
            {error, Errors}
    end;
verify_form({pp, _, {name, _, {symbol, Line, Name}}, Ins, Out = {def, _, _, _}}, State = #{vars := Vars}) ->
    {Ins1, [X, ':', _]= Out1} = subst(Ins, Out, Vars),
    Name1 = list_to_atom([$:|atom_to_list(Name)]),
    {ok,
     [{def, {Name, Line}, Ins1, Out1},
      def_equal(Name1, Line, X, X, Vars)],
     State};
verify_form({pp, _, {name, _, {symbol, Line, Name}}, Ins, Out = {assert, _, _}}, State) ->
    {ok, prove_equal('|->', Name, Line, Ins, Out, State), State};
verify_form({df, _, {name, _, {symbol, Line, Name}}, {def, _, _, _}=Def}, State = #{vars := Vars, df := Df}) ->
    [X, ':', Y] = subst_vars(Def, Vars),
    {ok, [def_equal(Name, Line, X, Y, Vars)], State#{df := Df ++ [{X, Y}]}};
verify_form({prop, _, {name, _, {symbol, Line, Name}}, Ins, Out, Rules}, State = #{defs := Defs}) ->
    [D1,{def, {Name1, _}, Ins1,_}=D2,P1] = prove_equal('|-<', Name, Line, Ins, Out, State),
    case apply_rules(Rules, Out, new_unify(Ins1), State) of
        {error, _} = Error ->
            Error;
        {ok, N, US} ->
            {N1, #{proven := Proven}} = unify_to_proof(Ins1, N, US, maps:to_list(Defs)),
            {ok,
             [D2, make_proof({Name1, Line}, length(Ins1), N1, Proven), D1, P1],
             State}
    end;
verify_form({dem, _, {name, _, {symbol, Line, Name}}, Ins, Out, Dem}, State = #{defs := Defs}) ->
    [D1,{def, {Name1, _}, Ins1,_}=D2,P1] = prove_equal('|-<', Name, Line, Ins, Out, State),

    case apply_dem(Dem, Out, new_unify(Ins1), State) of
        {error, _} = Error ->
            Error;
        {ok, N, US} ->
            {N1, #{proven := Proven}} = unify_to_proof(Ins1, N, US, maps:to_list(Defs)),
            {ok,
             [D2, make_proof({Name1, Line}, length(Ins1), N1, Proven), D1,P1],
             State}
    end.

validate_alias([], _) ->
    [];
validate_alias([{name, Line, {symbol, _, Name}}|T], Defs) ->
    case maps:find(Name, Defs) of
        {ok, _} ->
            validate_alias(T, Defs);
        error ->
            [{Line, ?MODULE, {def_not_found, Name}}|validate_alias(T, Defs)]
    end.

def_equal(Name, Line, X, Y, Vars) ->
    {def, {Name, Line}, equals(vars(X), Vars), [suffix(X, "1"), '=', suffix(Y, "2")]}.

equals(Vs, Vars) ->
    lists:append(
      [ begin
            V1 = suffix({var, V}, "1"),
            V2 = suffix({var, V}, "2"),
            Var = maps:get(V, Vars),
            [[V1, ':', Var],
             [V2, ':', Var],
             [V1, '=', V2]]
        end
       || V <- Vs]).

suffix([], _) ->
    [];
suffix([H|T], S) ->
    [suffix(H, S)|suffix(T, S)];
suffix({var, V}, S) ->
    {var, list_to_atom(atom_to_list(V) ++ S)};
suffix(X, _) ->
    X.


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

subst(Ins, Out, Vars) ->
    Ins1 = subst_vars(Ins, Vars),
    Out1 = subst_vars(Out, Vars),

    Ins2 =
        [[{var, V}, ':', maps:get(V, Vars)]
         || V <- vars([Out1|Ins1])] ++ Ins1,
    {Ins2, Out1}.

subst_vars({assert, _, P}, Vars) ->
    ['|-', subst_vars(P, Vars)];
subst_vars({def, _, X, Y}, Vars) ->
    [subst_vars(X, Vars), ':', subst_vars(Y, Vars)];
subst_vars([], _) ->
    [];
subst_vars([H|T], Vars) ->
    [subst_vars(H, Vars)|subst_vars(T, Vars)];
subst_vars(X, Vars) when is_atom(X) ->
    case maps:is_key(X, Vars) of
        true ->
            {var, X};
        false ->
            X
    end.


prove_equal(P, Name, Line, Ins, Out, #{vars := Vars, df := Df, defs := Defs}) ->
    {Ins1, Out1} = subst(Ins, Out, Vars),
    Ins2 = expand(Ins1, Df),
    Out2 = expand(Out1, Df),
    Name1 = list_to_atom([$@|atom_to_list(Name)]),

    {Name2, Name3, Ins3, P1, Out3} =
        case P of
            '|->' ->
                {Name1, Name, Ins2, '|-<', Out1};
            '|-<' ->
                {Name, Name1, Ins1, '|->', Out2}
        end,

    {ok, PS} = search_equals([Out1|Ins1], [Out2|Ins2], new_proof(Ins3), maps:to_list(Defs)),
    {In, PS1} = assert_equal(P1, Ins1, Ins2, PS),
    {_, PS2} = add_stmt(Out3, {{Name2, 0}, In}, PS1),
    {[N], #{proven := Proven}} = assert_equal(P, [Out1], [Out2], PS2),

    [{def, {Name1, Line}, Ins1, Out1},
     {def, {Name, Line}, Ins2, Out2},
     make_proof({Name3,Line}, length(Ins1), N, Proven)].


expand(L, Df) when is_list(L) ->
    expand_df([expand(E, Df) || E <- L], Df);
expand(X, _) ->
    X.

expand_df(L, []) ->
    L;
expand_df(L, [{X, Y}|T]) ->
    case russell_core:match(X, L, #{}) of
        {error, _} ->
            expand_df(L, T);
        {ok, Vars} ->
            {L1, Vars, 0} = russell_core:subst(Y, Vars, 0),
            L1
    end.


new_proof(Inputs) ->
    N = length(Inputs),
    Index = lists:seq(1, N),
    #{proven => #{},
      known => maps:from_list(lists:zip(Inputs, Index)),
      next_stmt => N+1}.

search_equals([], [], State, _) ->
    {ok, State};
search_equals([H|T1], [H|T2], State, Defs) ->
    search_equals(T1, T2, State, Defs);
search_equals([['|-', H1]|T1], [['|-', H2]|T2], State, Defs) ->
    {ok, _, State1} = russell_search:search([H1, '=', H2], State, Defs),
    search_equals(T1, T2, State1, Defs).

assert_equal(_, [], [], State) ->
    {[], State};
assert_equal(P, [H|T1], [H|T2], State = #{known := Known}) ->
    {T3, State1} = assert_equal(P, T1, T2, State),
    {[maps:get(H, Known)|T3], State1};
assert_equal(P, [['|-', H1]|T1], [['|-', H2]|T2], State = #{known := Known}) ->
    {X, Y} =
        case P of
            '|-<' ->
                {['|-', H2], ['|-', H1]};
            '|->' ->
                {['|-', H1], ['|-', H2]}
        end,
    A = maps:get(X, Known),
    E = maps:get([H1, '=', H2], Known),
    {N, State1} = add_stmt(Y, {{P, 0}, [A, E]}, State),
    {T3, State2} = assert_equal(P, T1, T2, State1),
    {[N|T3], State2}.


add_stmt(Stmt, Step, State = #{next_stmt := Next, known := Known, proven := Proven}) ->
    {Next, State#{next_stmt := Next+1, known := Known#{Stmt => Next}, proven := Proven#{Next => Step}}}.


new_unify(Ins) ->
    {_, Vars, Next} = russell_core:subst(Ins, #{}, 0),

    #{ next_var => Next,
       vars => Vars,
       next_stmt => length(Ins)+1,
       stmts => #{},
       steps => #{},
       subst => #{},
       names => #{}}.

add_stmt(Stmt, State = #{next_stmt := Next, stmts := Stmts}) ->
    {Next, State#{next_stmt => Next + 1, stmts => Stmts#{Next => Stmt}}}.

apply_rules({rules, Line, Rules}, Out, US = #{vars := Vars, names := Names}, State = #{defs := Defs, alias := Alias, vars := VarSubst, df := Df}) ->
    case resolve_subst(resolve_precedence([], Rules), VarSubst, Df, Vars) of
        {error, _} = Error ->
            Error;
        {ok, Tree} ->
            case resolve_ref(Tree, Names, Alias, Defs) of
                {error, _} = Error ->
                    Error;
                {ok, Choices} ->
                    case apply_rule_trees(Choices, Out, US, State) of
                        error ->
                            {error, {Line, ?MODULE, unification}};
                        {ok, _, _} = Result ->
                            Result
                    end
            end
    end.

resolve_precedence([{Op1, Pr1}, H1|T1], [H2, Op2={_, Pr2}|T2])
  when Pr1 =< Pr2 ->
    resolve_precedence(T1, [{Op1, H1, H2}, Op2|T2]);
resolve_precedence(Stack, [X, Y|T]) ->
    resolve_precedence([Y, X|Stack], T);
resolve_precedence([{Op, _}, X|T], [Y]) ->
    resolve_precedence(T, [{Op, X, Y}]);
resolve_precedence([], [X]) ->
    X.

resolve_subst({T, _, _}=X, _, _, _)
  when T =:= num; T =:= name; T =:= symbol->
    {ok, X};
resolve_subst({subst, _, X, Y}, VarSubst, Df, Vars) ->
    case make_subst(
           [{K, expand(subst_vars(V, VarSubst), Df)}
            || {K, V} <- Y],
           #{}, Vars)
    of
        {error, _} = Error ->
            Error;
        {ok, Y1} ->
            {ok, {subst, X, Y1}}
    end;
resolve_subst({Op, X, Y}, VarSubst, Df, Vars) ->
    case resolve_subst(X, VarSubst, Df, Vars) of
        {error, _} = Error ->
            Error;
        {ok, X1} ->
            case resolve_subst(Y, VarSubst, Df, Vars) of
                {error, _} = Error ->
                    Error;
                {ok, Y1} ->
                    {ok, {Op, X1, Y1}}
            end
    end.

make_subst([], Subst, Vars) ->
    {ok, maps:merge(Vars, Subst)};
make_subst([{{symbol, Line, V}, Token}|T], Subst, Vars) ->
    case russell_core:subst(Token, Vars, 0) of
        {Token1, Vars, 0} ->
            case maps:find(V, Subst) of
                {ok, _} ->
                    {error, {Line, ?MODULE, {subst_defined, V}}};
                error ->
                    make_subst(T, Subst#{V => Token1}, Vars)
            end;
        _ ->
            {error, {Line, ?MODULE, {unbound_var_found, V}}}
    end.

resolve_ref({num, Line, {symbol, _, Name}}, Names, _, _) ->
    case maps:is_key(Name, Names) of
        true ->
            {ok, [{num, Name}]};
        false ->
            {error, {Line, ?MODULE, {num_not_found, Name}}}
    end;
resolve_ref({name, Line, {symbol, _, Name}}, _, _, Defs) ->
    case maps:is_key(Name, Defs) of
        true ->
            {ok, [Name]};
        false ->
            {error, {Line, ?MODULE, {def_not_found, Name}}}
    end;
resolve_ref({symbol, Line, Name}, _, Alias, _) ->
    case maps:find(Name, Alias) of
        error ->
            {error, {Line, ?MODULE, {alias_not_found, Name}}};
        {ok, Names} ->
            {ok, Names}
    end;
resolve_ref({subst, X, Y}, Names, Alias, Defs) ->
    case resolve_ref(X, Names, Alias, Defs) of
        {error, _} = Error ->
            Error;
        {ok, X1} ->
            {ok, [{subst, X2, Y} || X2 <- X1]}
    end;
resolve_ref({Op, X, Y}, Names, Alias, Defs) ->
    case resolve_ref(X, Names, Alias, Defs) of
        {error, _} = Error ->
            Error;
        {ok, X1} ->
            case resolve_ref(Y, Names, Alias, Defs) of
                {error, _} = Error ->
                    Error;
                {ok, Y1} ->
                    {ok, [{Op, X2, Y2} || X2 <- X1, Y2 <- Y1]}
            end
    end.

apply_rule_trees([], _, _, _) ->
    error;
apply_rule_trees([H|T], Out, US, State) ->
    case apply_rule_tree(H, Out, US, State) of
        {ok, N, US1} ->
            {ok, N, US1};
        error ->
            apply_rule_trees(T, Out, US, State)
    end.

apply_rule_tree(Tree, Out, US, State = #{df := Df, vars := VarSubst}) ->
    {N, US1 = #{stmts := Stmts, vars := Vars, next_var := Next, subst := Subst}} = apply_rule(Tree, US, State),
    Out1 = expand(subst_vars(Out, VarSubst), Df),
    {Out2, Vars, Next}= russell_core:subst(Out1, Vars, Next),
    Out3 = maps:get(N, Stmts),
    case russell_unify:unify(Out2, Out3, Subst) of
        false ->
            error;
        Subst1 ->
            case validate_subst(Subst1, Vars) of
                false ->
                    error;
                true ->
                    {ok, N, US1#{subst := Subst1}}
            end
    end.

validate_subst(Subst, Vars) ->
    Vars1 = maps:map(fun(_, V) -> russell_unify:subst(V, Subst) end, Vars),
    case lists:all(fun({var, X}) when is_integer(X) -> true; (_) -> false end, maps:values(Vars1)) of
        false ->
            false;
        true ->
            Vars2 = maps:to_list(Vars1),
            Subst1 =
                maps:merge(
                  maps:from_list([{I, {var,K}} || {K,{var,I}} <- Vars2]),
                  Subst),
            lists:all(fun({K,V}) -> russell_unify:subst(V, Subst1) =:= {var, K} end, Vars2)
    end.

apply_rule({1, X, Y}, US, State = #{defs := Defs}) ->
    {X1, US1} = apply_rule(X, US, State),
    {Y1, US2} = apply_rule(Y, US1, State),
    apply_def({'1.1', [X1, Y1]}, US2, Defs);
apply_rule({2, X, Y}, US, State = #{defs := Defs}) ->
    {X1, US1} = apply_rule(X, US, State),
    {Y1, US2} = apply_rule(Y, US1, State),
    apply_def({'1.1', [X1, {'1.1', [Y1, {'2.05', []}]}]}, US2, Defs);
apply_rule({num, Num}, State = #{names := Names}, _) ->
    {maps:get(Num, Names), State};
apply_rule({subst, Name, Subst}, State, #{defs := Defs}) ->
    apply_def(Name, [], Subst, State, Defs);
apply_rule(Name, State, #{defs := Defs}) when is_atom(Name) ->
    apply_def(Name, [], #{}, State, Defs).

apply_def({Name, Ins}, State, Defs) ->
    {Ins1, State1} = apply_defs(Ins, State, Defs),
    apply_def(Name, Ins1, #{}, State1, Defs);
apply_def(X, State, _) ->
    {X, State}.

apply_defs([], State, _) ->
    {[], State};
apply_defs([H|T], State, Defs) ->
    {H1, State1} = apply_def(H, State, Defs),
    {T1, State2} = apply_defs(T, State1, Defs),
    {[H1|T1], State2}.

apply_def(Name, Inputs, Subst, State, Defs) ->
    {Ins, Out, State1 = #{stmts := Stmts, subst := Subst1}} = subst_def(Name, Subst, State, Defs),
    Inputs1 = [maps:get(I, Stmts) || I <- Inputs],
    Ins1 = lists:nthtail(length(Ins) - length(Inputs), Ins),
    Subst2 = russell_unify:unify(Inputs1, Ins1, Subst1),
    add_step(Name, Ins, Out, State1#{subst := Subst2}).

add_step(Name, Ins, Out, State) ->
    {Ins1, State1} = lists:mapfoldl(fun add_stmt/2, State, Ins),
    {Out1, State2 = #{steps := Steps}} = add_stmt(Out, State1),
    {Out1, State2#{steps := Steps#{Out1 => {Name, Ins1}}}}.

subst_def(Name, Subst, State = #{next_var := Next}, Defs) ->
    {Ins, Out} = maps:get(Name, Defs),
    {Ins1, Subst1, Next1} = russell_core:subst(Ins, Subst, Next),
    {Out1, _, Next2} = russell_core:subst(Out, Subst1, Next1),
    {Ins1, Out1, State#{next_var := Next2}}.


unify_to_proof(Ins, Out, #{vars := Vars, subst := Subst, next_stmt := Next, stmts := Stmts, steps := Steps}, Defs) ->
    Subst1 =
        maps:merge(
          maps:from_list(
            [ begin
                  {var, V1} = russell_unify:subst(V, Subst),
                  {V1, {var, K}}
              end
              || {K, V} <- maps:to_list(Vars) ]),
          Subst),

    {PS, Map} =
        lists:foldl(
          fun(N, {PS, Map}) ->
                  S = russell_unify:subst(maps:get(N, Stmts), Subst1),
                  case maps:find(N, Steps) of
                      error ->
                          {ok, N1, PS1} = russell_search:search(S, PS, Defs);
                      {ok, {Name, In}} ->
                          {N1, PS1} = add_stmt(S, {{Name, 0}, [maps:get(I, Map) || I <- In]}, PS)
                  end,
                  {PS1, Map#{N => N1}}
          end,
          {new_proof(Ins), #{}},
          lists:seq(length(Ins) + 1, Next - 1)),
    {maps:get(Out, Map), PS}.

apply_dem([Steps], Out, US, State) ->
    apply_steps(fix_prop(Steps, Out), US, State);
apply_dem([{{num, _, {symbol, _, Num}}, Steps}|T], O, US, State) ->
    case apply_steps(Steps, US, State) of
        {error, _} = Error ->
            Error;
        {ok, N, US1 = #{names := Names}} ->
            apply_dem(T, O, US1#{names := Names#{Num => N}}, State)
    end.

fix_prop([], _) ->
    [];
fix_prop([{Rules, {'.Prop', _}}], Out) ->
    [{Rules, Out}];
fix_prop([H|T], Out) ->
    [H|fix_prop(T, Out)].

apply_steps([{Rules, {assert, _, P}=Out}|T], US, State) ->
    case apply_rules(Rules, Out, US, State) of
        {error, _} = Error ->
            Error;
        {ok, N, US1} ->
            apply_steps1(T, N, P, US1, State)
    end;
apply_steps([{Rules, P, {symbol, _, Sym}, Q}|T], US, State) ->
    case apply_rules(Rules, ['|-', [P, Sym, Q]], US, State) of
        {error, _} = Error ->
            Error;
        {ok, N, US1} ->
            apply_steps2(T, N, Q, US1, State)
    end.

apply_steps1([], N, _, US, _) ->
    {ok, N, US};
apply_steps1([{symbol, _, Sym}, {Rules, Q}|T], N, P, US, State = #{defs := Defs}) ->
    case apply_rules(Rules, ['|-', [P, Sym, Q]], US, State) of
        {error, _} = Error ->
            Error;
        {ok, N1, US1} ->
            {N2, US2} = apply_def({'1.1', [N, N1]}, US1, Defs),
            apply_steps1(T, N2, Q, US2, State)
    end.

apply_steps2([], N, _, US, _) ->
    {ok, N, US};
apply_steps2([{symbol, _, Sym}, {Rules, Q}|T], N, P, US, State = #{defs := Defs}) ->
    case apply_rules(Rules, ['|-', [P, Sym, Q]], US, State) of
        {error, _} = Error ->
            Error;
        {ok, N1, US1} ->
            {N4, US4} = apply_def({'1.1', [N, {'1.1', [N1, {'2.05', []}]}]}, US1, Defs),
            apply_steps2(T, N4, Q, US4, State)
    end.

make_proof(Name, NIn, N, Steps) ->
    {proof, {Name, [make_name(I) || I <- lists:seq(1, NIn)]},
     [make_stmt(I, maps:get(I, Steps)) || I <- lists:seq(NIn+1,N)]}.

make_name(I) ->
    {list_to_atom(integer_to_list(I)), 1}.

make_stmt(Out, {Name, Ins}) ->
    {Name, [make_name(I) || I <- [Out|Ins]]}.
