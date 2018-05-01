-module(russell_dem).

-export([run/1, format_error/1]).

run([Filename, Output]) ->
    case parse(Filename) of
        {ok, Forms} ->
            case russell:file_error(Filename, verify_forms(Forms)) of
                {error, _} = Error ->
                    Error;
                {ok, Forms1} ->
                    ok = file:write_file(Output, russell_prim:format(Forms1))
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
          df => []},

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
verify_form({pp, _, {name, _, {symbol, Line, Name}}, Ins, Out = {def, _, _, _}}, State = #{vars := Vars}) ->
    Ins1 = subst_vars(Ins, Vars),
    [X, ':', _] = Out1 = subst_vars(Out, Vars),

    Ins2 =
        [[{var, V}, ':', maps:get(V, Vars)]
         || V <- vars([Out1|Ins1])] ++ Ins1,

    Name1 = list_to_atom([$:|atom_to_list(Name)]),

    Ins3 = equals(vars(X), Vars),
    Out2 = [suffix(X, "1"), '=', suffix(X, "2")],

    {ok,
     [{def, {Name, Line}, Ins2, Out1},
      {def, {Name1, Line}, Ins3, Out2}],
     State};
verify_form({pp, _, {name, _, {symbol, Line, Name}}, Ins, Out = {assert, _, _}}, State = #{vars := Vars, df := Df, defs := Defs}) ->
    Ins1 = subst_vars(Ins, Vars),
    Out1 = subst_vars(Out, Vars),

    Ins2 =
        [[{var, V}, ':', maps:get(V, Vars)]
         || V <- vars([Out1|Ins1])] ++ Ins1,

    Ins3 = expand(Ins2, Df),
    Out2 = expand(Out1, Df),

    Name1 = list_to_atom([$@|atom_to_list(Name)]),
    {ok, P} = search_equals([Out1|Ins2], [Out2|Ins3], new_proof(Ins3), maps:to_list(Defs)),
    {In, P1} = assert_input_equals(Ins2, Ins3, P),
    {_, P2} = add_stmt(Out1, {{Name, 0}, In}, P1),
    {N, #{proven := Proven}} = assert_output_equal(Out1, Out2, P2),

    {ok,
     [{def, {Name, Line}, Ins2, Out1},
      {def, {Name1, Line}, Ins3, Out2},
      make_proof({Name1,Line}, length(Ins2), N, Proven)],
     State};
verify_form({df, _, {name, _, {symbol, Line, Name}}, {def, _, X, Y}}, State = #{vars := Vars, df := Df}) ->
    X1 = subst_vars(X, Vars),
    Y1 = subst_vars(Y, Vars),

    Ins = equals(vars(X1), Vars),
    Out = [suffix(X1, "1"), '=', suffix(Y1, "2")],

    {ok, [{def, {Name, Line}, Ins, Out}], State#{df := Df ++ [{X1, Y1}]}}.


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


assert_input_equals([], [], State) ->
    {[], State};
assert_input_equals([H|T1], [H|T2], State = #{known := Known}) ->
    H1 = maps:get(H, Known),
    {T3, State1} = assert_input_equals(T1, T2, State),
    {[H1|T3], State1};
assert_input_equals([['|-', H1]|T1], [['|-', H2]|T2], State = #{known := Known}) ->
    A = maps:get(['|-', H2], Known),
    E = maps:get([H1, '=', H2], Known),
    {N, State1} = add_stmt(['|-', H1], {{'|-<', 0}, [A, E]}, State),
    {T3, State2} = assert_input_equals(T1, T2, State1),
    {[N|T3], State2}.

assert_output_equal(X, X, State = #{known := Known}) ->
    {maps:get(X, Known), State};
assert_output_equal(['|-',X1], ['|-',X2], State = #{known := Known}) ->
    A = maps:get(['|-', X1], Known),
    E = maps:get([X1, '=', X2], Known),
    add_stmt(['|-', X1], {{'|->', 0}, [A, E]}, State).

add_stmt(Stmt, Step, State = #{next_stmt := Next, known := Known, proven := Proven}) ->
    {Next, State#{next_stmt := Next+1, known := Known#{Stmt => Next}, proven := Proven#{Next => Step}}}.


make_proof(Name, NIn, N, Steps) ->
    {proof, {Name, [make_name(I) || I <- lists:seq(1, NIn)]},
     [make_stmt(I, maps:get(I, Steps)) || I <- lists:seq(NIn+1,N)]}.

make_name(I) ->
    {list_to_atom(integer_to_list(I)), 1}.

make_stmt(Out, {Name, Ins}) ->
    {Name, [make_name(I) || I <- [Out|Ins]]}.
