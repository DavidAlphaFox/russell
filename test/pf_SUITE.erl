-module(pf_SUITE).

-include_lib("common_test/include/ct.hrl").
-compile(export_all).

all() ->
    [parse_error,
     validate_head_stmt_defined,
     validate_head_stmt_not_found,
     validate_body_stmt_defined,
     validate_body_stmt_not_found,
     pf_head_def_not_found,
     pf_body_def_not_found,
     pf_head_input_mismatch,
     pf_head_output_mismatch,
     pf_body_input_mismatch,
     pf_body_output_mismatch,
     stmt_symbol_mismatch,
     stmt_var_mismatch,
     unbound,
     demo0].

parse_def(C, F) ->
    russell_def:file(filename:join(?config(data_dir, C), F)).

parse_pf(C, F) ->
    russell_pf:file(filename:join(?config(data_dir, C), F)).

verify(C, D, P) ->
    {ok, Def} = parse_def(C, D),
    {ok, Pf} = parse_pf(C, P),
    russell_pf:verify(Def, Pf).


parse_error(C) ->
    {error, {1, russell_pf_parser, _}} =
        parse_pf(C, "parse_error.pf"),
    ok.

validate_head_stmt_defined(C) ->
    {error, {1, russell_pf, {stmt_defined, a}}} =
        parse_pf(C, "head_stmt_defined.pf"),
    ok.

validate_head_stmt_not_found(C) ->
    {error, {1, russell_pf, {stmt_not_found, a}}} =
        parse_pf(C, "head_stmt_not_found.pf"),
    ok.

validate_body_stmt_defined(C) ->
    {error, {2, russell_pf, {stmt_defined, a}}} =
        parse_pf(C, "body_stmt_defined.pf"),
    ok.

validate_body_stmt_not_found(C) ->
    {error, {2, russell_pf, {stmt_not_found, b}}} =
        parse_pf(C, "body_stmt_not_found.pf"),
    ok.

pf_head_def_not_found(C) ->
    {ok, P} = parse_pf(C, "def_not_found.pf"),
    {error, {def_not_found, p}} = russell_pf:verify(#{}, P).

pf_body_def_not_found(C) ->
    {error, {_, {def_not_found, q}}, _, _} =
        verify(C, "def_not_found.def", "def_not_found.pf"),
    ok.

pf_head_input_mismatch(C) ->
    {error,{input_stmt_number_mismatch,2,1}} =
        verify(C, "head_input_mismatch.def", "def_not_found.pf"),
    ok.

pf_head_output_mismatch(C) ->
    {error,{output_stmt_number_mismatch,2,1}} =
        verify(C, "head_output_mismatch.def", "def_not_found.pf"),
    ok.

pf_body_input_mismatch(C) ->
    {error,{_, {input_stmt_number_mismatch,1,2}}, _, _} =
        verify(C, "body_input_mismatch.def", "def_not_found.pf"),
    ok.

pf_body_output_mismatch(C) ->
    {error,{_, {output_stmt_number_mismatch,2,1}}, _, _} =
        verify(C, "body_output_mismatch.def", "def_not_found.pf"),
    ok.

stmt_symbol_mismatch(C) ->
    {error,{_, {not_match,q,p}}, _, _} =
        verify(C, "symbol_mismatch.def", "mismatch.pf"),
    ok.

stmt_var_mismatch(C) ->
    {error,{_, {not_match,q,p}}, _, _} =
        verify(C, "var_mismatch.def", "mismatch.pf"),
    ok.

unbound(C) ->
    {ok, _, _} = verify(C, "unbound.def", "unbound.pf"),
    ok.

demo0(C) ->
    {ok, _, _} = verify(C, "demo0.def", "demo0.pf"),
    ok.
