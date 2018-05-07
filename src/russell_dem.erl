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

format_error({file_not_exist, Filename}) ->
    io_lib:format("~ts: file not exist", [Filename]).


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
verify_form({alias, _, {symbol, _, Name}, Names}, State = #{alias := Alias}) ->
    {ok, [], State#{alias := Alias#{Name => [N || {name, _, {symbol, _, N}} <- Names]}}};
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
verify_form({prop, _, {name, _, {symbol, Line, Name}}, Ins, Out, Rule}, State = #{defs := Defs}) ->
    [{def, _, _, Out1}=D1,{def, {Name1, _}, Ins1,_}=D2,P1] = prove_equal('|-<', Name, Line, Ins, Out, State),

    {ok, N, US} = apply_rules(Rule, Out1, new_unify(Ins1), State),

    {N1, #{proven := Proven}} = unify_to_proof(Ins1, N, US, maps:to_list(Defs)),

    {ok,
     [D2, make_proof({Name1, Line}, length(Ins1), N1, Proven), D1, P1],
     State}.

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
    case russell_search:search([H1, '=', H2], State, Defs) of
        {ok, _, State1} ->
            search_equals(T1, T2, State1, Defs);
        {not_found, _} ->
            {error, not_found}
    end.

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
       subst => #{}}.

add_stmt(Stmt, State = #{next_stmt := Next, stmts := Stmts}) ->
    {Next, State#{next_stmt => Next + 1, stmts => Stmts#{Next => Stmt}}}.

apply_rules(Rule, Out, US, State = #{df := Df}) ->
    {ok, N, US1 = #{stmts := Stmts, vars := Vars, next_var := Next, subst := Subst}} = apply_rule(Rule, US, State),
    Out1 = expand(Out, Df),
    {Out2, Vars, Next}= russell_core:subst(Out1, Vars, Next),
    Out3 = maps:get(N, Stmts),
    Subst1 = russell_unify:unify(Out2, Out3, Subst),
    {ok, N, US1#{subst := Subst1}}.


apply_rule({subst, _, {symbol,_,Name}, Subst}, State = #{vars := Vars, next_var := Next}, #{alias := Alias, defs := Defs, vars := VarSubst}) ->
    [Name1] = maps:get(Name, Alias),
    {ok, Subst1} = make_subst(Subst, #{}, Vars, VarSubst),
    {Ins, Out} = maps:get(Name1, Defs),
    {Ins1, Subst2, Next1} = russell_core:subst(Ins, Subst1, Next),
    {Out1, _, Next2} = russell_core:subst(Out, Subst2, Next1),
    {Ins2, State1} = lists:mapfoldl(fun add_stmt/2, State#{next_var := Next2}, Ins1),
    {Out2, State2 = #{steps := Steps}} = add_stmt(Out1, State1),
    {ok, Out2, State2#{steps := Steps#{Out2 => {Name1, Ins2}}}}.


make_subst([], Subst, Vars, _) ->
    {ok, maps:merge(Vars, Subst)};
make_subst([{{symbol, Line, V}, Token}|T], Subst, Vars, VarSubst) ->
    case russell_core:subst(subst_vars(Token, VarSubst), Vars, 0) of
        {Token1, Vars, 0} ->
            case maps:find(V, Subst) of
                {ok, _} ->
                    {error, {Line, ?MODULE, {subst_defined, V}}};
                error ->
                    make_subst(T, Subst#{V => Token1}, Vars, VarSubst)
            end;
        _ ->
            {error, {Line, ?MODULE, {unbound_var_found, V}}}
    end.

unify_to_proof(Ins, Out, #{vars := Vars, subst := Subst, next_stmt := Next, stmts := Stmts, steps := Steps}, Defs) ->
    Subst1 =
        maps:merge(
          maps:from_list(
            [ {V, {var, K}}
              || {K, {var, V}} <- maps:to_list(Vars) ]),
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


make_proof(Name, NIn, N, Steps) ->
    {proof, {Name, [make_name(I) || I <- lists:seq(1, NIn)]},
     [make_stmt(I, maps:get(I, Steps)) || I <- lists:seq(NIn+1,N)]}.

make_name(I) ->
    {list_to_atom(integer_to_list(I)), 1}.

make_stmt(Out, {Name, Ins}) ->
    {Name, [make_name(I) || I <- [Out|Ins]]}.
