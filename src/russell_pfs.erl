-module(russell_pfs).

-export([run/1, format_error/1]).

run([DFN, SFN]) ->
    {ok, Name, Resolved, Defs} = russell:file_error(SFN, resolve(DFN, SFN)),
    {ok, Pf} = russell:file_error(SFN, construct(Name, Resolved, Defs)),
    io:format("~s~n", [russell_pf:format(Pf)]);
run([DFN, SFN, PFN]) ->
    {ok, Name, Resolved, Defs} = russell:file_error(SFN, resolve(DFN, SFN)),
    {ok, Pf} = russell:file_error(SFN, construct(Name, Resolved, Defs)),
    file:write_file(PFN, russell_pf:format(Pf)).

file(Filename) ->
    {ok, Bin} = file:read_file(Filename),
    parse(binary_to_list(Bin)).

parse(String) ->
    {ok, Tokens, _} = russell_lexer:string(String),
    russell_pfs_parser:parse(Tokens).

format_error({unbound_var_found, V}) ->
    io_lib:format("unbound variable found in substitution of ~s", [V]);
format_error({subst_defined, V}) ->
    io_lib:format("substituion of ~s already defined", [V]);
format_error({unbound_var_found, N, I}) ->
    io_lib:format("unbound variable found in input ~B of ~s", [I, N]);
format_error({not_match, N, I, E, G}) ->
    io_lib:format(
      "statement mismatch in input ~B of ~s  Expected:~n    ~s~n  Got:~n    ~s",
      [I, N, russell_def:format_tokens(E),russell_def:format_tokens(G)]);
format_error({output_mismatch, E, G}) ->
    io_lib:format(
      "output statement mismatch~n  Expected:~n    ~s~n  Got:~n    ~s",
      [russell_def:format_tokens(E),russell_def:format_tokens(G)]);
format_error({not_found, N, I, Stmt}) ->
    io_lib:format("input ~B of ~s not found:~n  ~s", [I, N, russell_def:format_tokens(Stmt)]).


resolve(DFN, SFN) ->
    {ok, Defs} = russell:file_error(DFN, russell_def:file(DFN)),
    {ok, {{_,Line}=Name, Ins, Steps}} = russell:file_error(SFN, file(SFN)),
    {ok, {InStmts, OutStmt}} = russell:file_error(SFN, russell_def:find(Name, length(Ins), Defs)),

    {InStmts1, Vars, NextVar} = russell_def:subst(InStmts, #{}, 0),
    {OutStmt1, _, NextVar1} = russell_def:subst(OutStmt, Vars, NextVar),

    State =
        #{ next_var => NextVar1,
           vars => Vars,
           next_stmt => 1,
           stmts => #{},
           steps => #{},
           subst => #{}},

    {InStmts2, State1} = add_stmts(InStmts1, State),
    InStmts3 = lists:sublist(InStmts2, length(Ins)),

    {ok, Names} = russell:file_error(SFN, bind_names(lists:zip(Ins, InStmts3), #{})),

    {ok, #{steps := Steps1, subst := Subst, stmts := Stmts, next_stmt := Next, names := Names1}} =
        russell:file_error(SFN, resolve_steps(Steps, State1#{names => Names}, Defs)),

    Out = maps:get(Next-1, Stmts),
    case russell_unify:unify(OutStmt1, Out, Subst) of
        false ->
            {error, {Line, ?MODULE, {output_mismatch, OutStmt, russell_unify:subst(Out, Subst)}}};
        Subst1 ->
            Subst2 =
                lists:foldl(
                  fun ({K, V}, S) ->
                          {var, V1} = russell_unify:subst(V, S),
                          S#{V1 => {var, K}}
                  end,
                  Subst1,
                  maps:to_list(Vars)),

            Stmts1 = maps:map(fun(_, V) -> russell_unify:subst(V, Subst2) end, Stmts),

            lists:foreach(
              fun({N,V}) ->
                      io:format(
                        "(~s) ~s~n",
                        [N,russell_def:format_tokens(maps:get(V, Stmts1))])
              end,
              maps:to_list(Names1)),

            {ok,
             Name,
             #{out => Next-1,
               inputs => InStmts,
               steps => Steps1,
               stmts => Stmts1},
             Defs}
    end.

resolve_steps([], State, _) ->
    {ok, State};
resolve_steps([{Out, H}|T], State, Defs) ->
    case resolve_step(H, State, Defs) of
        {error, _} = Error ->
            Error;
        {ok, Stmt, State1=#{names := Names}} ->
            case bind_name(Out, Stmt, Names) of
                {error, _} = Error ->
                    Error;
                {ok, Names1} ->
                    resolve_steps(T, State1#{names := Names1}, Defs)
            end
    end.


resolve_step({'_', _}, State, _) ->
    {ok, '_', State};
resolve_step({Name, Line}, State=#{names := Names}, _) ->
    case maps:find(Name, Names) of
        error ->
            {error, {Line, russell_pf, {stmt_not_found, Name}}};
        {ok, N} ->
            {ok, N, State}
    end;
resolve_step({apply, Name={_,Line}, Ins}, State = #{next_var := Next}, Defs) ->
    case russell_def:find(Name, length(Ins), Defs) of
        {error, _} = Error ->
            Error;
        {ok, {InStmts, OutStmt}} ->
            Ins1 =
                if length(Ins) < length(InStmts) ->
                        lists:duplicate(length(InStmts) - length(Ins), {'_', Line}) ++ Ins;
                   true ->
                        Ins
                end,

            {InStmts1, Vars, Next1} = russell_def:subst(InStmts, #{}, Next),
            {OutStmt1, _, Next2} = russell_def:subst(OutStmt, Vars, Next1),

            case resolve_inputs(1, Ins1, InStmts1, State#{next_var := Next2}, Name, Defs) of
                {error, _} = Error ->
                    Error;
                {ok, Ins2, State1} ->
                    {Out, State2 = #{steps := Steps}} = add_stmt(OutStmt1, State1),
                    {ok, Out, State2#{steps := Steps#{Out => {Name, Ins2}}}}
            end
    end;
resolve_step({subst, Name, Subst}, State = #{vars := Vars, next_var := Next}, Defs) ->
    case russell_def:find(Name, Defs) of
        {error, _} = Error ->
            Error;
        {ok, {Ins, Out}} ->
            case make_subst(Subst, #{}, Vars) of
                {error, _} = Error ->
                    Error;
                {ok, Subst1} ->
                    Subst2 = maps:merge(Vars, Subst1),
                    {Ins1, Subst3, Next1} = russell_def:subst(Ins, Subst2, Next),
                    {Out1, _, Next2} = russell_def:subst(Out, Subst3, Next1),

                    {Ins2, State1} = add_stmts(Ins1, State#{next_var := Next2}),
                    {Out2, State2 = #{steps := Steps}} = add_stmt(Out1, State1),
                    {ok, Out2, State2#{steps := Steps#{Out2 => {Name, Ins2}}}}
            end
    end.

resolve_inputs(_, [], [], State, _, _) ->
    {ok, [], State};
resolve_inputs(N, [H|T], [SH|ST], State, Name, Defs) ->
    case resolve_step(H, State, Defs) of
        {error, _} = Error ->
            Error;
        {ok, H1, State1} ->
            case unify(N, H1, SH, State1, Name) of
                {error, _} = Error ->
                    Error;
                {ok, H2, State2} ->
                    case resolve_inputs(N+1, T, ST, State2, Name, Defs) of
                        {error, _} = Error ->
                            Error;
                        {ok, T1, State3} ->
                            {ok, [H2|T1], State3}
                    end
            end
    end.


bind_names([], Names) ->
    {ok, Names};
bind_names([{Name, Value}|T], Names) ->
    case bind_name(Name, Value, Names) of
        {error, _} = Error ->
            Error;
        {ok, Names1} ->
            bind_names(T, Names1)
    end.

bind_name({Name, Line}, Value, Names) ->
    case maps:find(Name, Names) of
        {ok, _} ->
            {error, {Line, russell_pf, {stmt_defined, Name}}};
        error ->
            {ok, Names#{Name => Value}}
    end.

add_stmts(Stmts, State) ->
    lists:mapfoldl(fun add_stmt/2, State, Stmts).

add_stmt(Stmt, State = #{next_stmt := Next, stmts := Stmts}) ->
    {Next, State#{next_stmt => Next + 1, stmts => Stmts#{Next => Stmt}}}.


make_subst([], Subst, _) ->
    {ok, Subst};
make_subst([{{V, Line}, Token}|T], Subst, Vars) ->
    case russell_def:subst(Token, Vars, 0) of
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


unify(_, '_', Y, State, _) ->
    {N, State1} = add_stmt(Y, State),
    {ok, N, State1};
unify(N, X, Y, State=#{subst := Subst, stmts := Stmts}, {Name, Line}) ->
    Stmt = maps:get(X, Stmts),
    case russell_unify:unify(Stmt, Y, Subst) of
        false ->
            {error, {Line, ?MODULE, {not_match, Name, N, russell_unify:subst(Stmt, Subst), russell_unify:subst(Y, Subst)}}};
        Subst1 ->
            {ok, X, State#{subst := Subst1}}
    end.


construct(Name, #{inputs := Inputs, out := Out, steps := Proved, stmts := Stmts}, Defs) ->
    NIn = length(Inputs),
    Index = lists:seq(1,NIn),

    State = #{
      proved => maps:merge(maps:from_list([{I,I} || I <- Index]), Proved),
      known => maps:from_list(lists:zip(Inputs, Index)),
      next_stmt => Out + 1,
      steps => #{},
      next_step => length(Inputs)+1},

    case construct_stmt(Out, State, {{'_',0}, 0}, Stmts, maps:to_list(Defs)) of
        {error, _} = Error ->
            Error;
        {ok, N, #{steps := Steps}} ->
            {ok, make_proof(Name, NIn, N, Steps)}
    end.


construct_stmt(N, State = #{proved := Proved}, {{Name,Line}, I}, Stmts, Defs) ->
    case maps:find(N, Proved) of
        error ->
            Stmt = maps:get(N, Stmts),
            case validate(Stmt, {Name,Line}, I) of
                {error, _} = Error ->
                    Error;
                ok ->
                    case russell_search:search(Stmt, State, Defs) of
                        {not_found, _} ->
                            {error, {Line, ?MODULE, {not_found, Name, I, Stmt}}};
                        {ok, N1, State1} ->
                            construct_stmt(N1, State1, {{Name,Line},I}, Stmts, Defs)
                    end
            end;
        {ok, N1} when is_integer(N1) ->
            {ok, N1, State};
        {ok, {Name1, Ins}} ->
            case construct_stmts(1, Ins, State, Name1, Stmts, Defs) of
                {error, _} = Error ->
                    Error;
                {ok, Ins1, State1} ->
                    {N1, State2 = #{known := Known, proved := Proved1}} = add_step(Name1, Ins1, State1),

                    State3 =
                        case maps:find(N, Stmts) of
                            error ->
                                State2;
                            {ok, Stmt} ->
                                State2#{known := Known#{Stmt => N}}
                        end,

                    {ok, N1, State3#{proved := Proved1#{N := N1}}}
            end
    end.

add_step(Name, Ins, State = #{next_step := Next, steps := Steps}) ->
    {Next, State#{next_step := Next + 1, steps := Steps#{Next => {Name, Ins}}}}.


construct_stmts(_, [], State, _, _, _) ->
    {ok, [], State};
construct_stmts(N, [H|T], State, Name, Stmts, Defs) ->
    case construct_stmt(H, State, {Name, N}, Stmts, Defs) of
        {error, _} = Error ->
            Error;
        {ok, H1, State1} ->
            case construct_stmts(N+1, T, State1, Name, Stmts, Defs) of
                {error, _} = Error ->
                    Error;
                {ok, T1, State2} ->
                    {ok, [H1|T1], State2}
            end
    end.


validate([], _, _) ->
    ok;
validate([H|T], Name, I) ->
    case validate(H, Name, I) of
        {error, _} = Error ->
            Error;
        ok ->
            validate(T, Name, I)
    end;
validate({var, X}, {Name, Line}, I) when is_integer(X) ->
    {error, {Line, ?MODULE, {unbound_var_found, Name, I}}};
validate(_, _, _) ->
    ok.


make_proof(Name, NIn, N, Steps) ->
    {Name,
     [make_name(I) || I <- lists:seq(1, NIn)],
     [make_stmt(I, maps:get(I, Steps)) || I <- lists:seq(NIn+1,N)]
    }.

make_name(I) ->
    {list_to_atom(integer_to_list(I)), 1}.

make_stmt(Out, {Name, Ins}) ->
    {Name, [make_name(I) || I <- Ins], make_name(Out)}.
