-module(pf_SUITE).

-include_lib("common_test/include/ct.hrl").
-compile(export_all).

all() ->
    [def_parse_error,
     pf_parse_error,
     pf_format,
     validate_head_stmt_defined,
     validate_body_stmt_defined,
     validate_body_stmt_not_found,
     pf_head_def_not_found,
     pf_body_def_not_found,
     pf_head_input_mismatch,
     pf_body_input_mismatch,
     pf_output_stmt_mismatch,
     stmt_symbol_mismatch,
     stmt_var_mismatch,
     unbound,
     demo0].

parse_def(C, F) ->
    DFN = filename:join(?config(data_dir, C), F),
    PFN = filename:join(?config(data_dir, C), "parse_error.pf"),
    catch russell_verify:run([DFN, PFN]),
    russell_def:file(DFN).

parse_pf(C, F) ->
    DFN = filename:join(?config(data_dir, C), "demo0.def"),
    PFN = filename:join(?config(data_dir, C), F),
    catch russell_verify:run([DFN, PFN]),
    russell_pf:file(PFN).

verify(C, D, P) ->
    DFN = filename:join(?config(data_dir, C), D),
    PFN = filename:join(?config(data_dir, C), P),

    {ok, Def} = russell_def:file(DFN),
    {ok, Pf} = russell_pf:file(PFN),
    catch russell_verify:run([DFN,PFN]),
    russell_pf:verify(Def, Pf).

def_parse_error(C) ->
    {error, {1, russell_def_parser, _}} =
        parse_def(C, "parse_error.def"),
    ok.

pf_parse_error(C) ->
    {error, {1, russell_pf_parser, _}} =
        parse_pf(C, "parse_error.pf"),
    ok.

validate_head_stmt_defined(C) ->
    {error, {1, russell_pf, {stmt_defined, a}}} =
        parse_pf(C, "head_stmt_defined.pf"),
    ok.

validate_body_stmt_defined(C) ->
    {error, {2, russell_pf, {stmt_defined, a}}} =
        parse_pf(C, "body_stmt_defined.pf"),
    ok.

validate_body_stmt_not_found(C) ->
    {error, {2, russell_pf, {stmt_not_found, a}}} =
        parse_pf(C, "body_stmt_not_found.pf"),
    ok.

pf_head_def_not_found(C) ->
    {ok, P} = parse_pf(C, "def_not_found.pf"),
    {error, {1, russell_def, {def_not_found, p}}} = russell_pf:verify(#{}, P).

pf_body_def_not_found(C) ->
    {error, {2, russell_def, {def_not_found, q}}, _, _} =
        verify(C, "def_not_found.def", "def_not_found.pf"),
    ok.

pf_head_input_mismatch(C) ->
    {error,{1, russell_def, {input_number_mismatch,2,1}}} =
        verify(C, "head_input_mismatch.def", "def_not_found.pf"),
    ok.

pf_head_output_mismatch(C) ->
    {error, {1, russell_pf, {output_number_mismatch,2,1}}} =
        verify(C, "head_output_mismatch.def", "def_not_found.pf"),
    ok.

pf_body_input_mismatch(C) ->
    {error,{2, russell_def, {input_number_mismatch,1,2}}, _, _} =
        verify(C, "body_input_mismatch.def", "def_not_found.pf"),
    ok.

pf_body_output_mismatch(C) ->
    {error,{2, russell_pf, {output_number_mismatch,2,1}}, _, _} =
        verify(C, "body_output_mismatch.def", "def_not_found.pf"),
    ok.

pf_output_stmt_mismatch(C) ->
    {error,{2, russell_def, {not_match,_,_}}, _, _} =
        verify(C, "output_stmt_mismatch.def", "def_not_found.pf"),
    ok.

stmt_symbol_mismatch(C) ->
    {error,{3, russell_def, {not_match,_,_}}, _, _} =
        verify(C, "symbol_mismatch.def", "mismatch.pf"),
    ok.

stmt_var_mismatch(C) ->
    {error,{3, russell_def, {not_match,_,_,_,_}}, _, _} =
        verify(C, "var_mismatch.def", "mismatch.pf"),
    ok.

unbound(C) ->
    {ok, _, _} = verify(C, "unbound.def", "unbound.pf"),
    ok.

demo0(C) ->
    {ok, _, _} = verify(C, "demo0.def", "demo0.pf"),
    ok.

pf_format(C) ->
    PFN = filename:join(?config(data_dir, C), "demo0.pf"),
    {ok, P} = russell_pf:file(PFN),
    {ok, Bin} = file:read_file(PFN),
    Bin = iolist_to_binary(russell_pf:format(P)),
    ok.
