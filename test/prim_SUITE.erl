-module(prim_SUITE).

-include_lib("common_test/include/ct.hrl").
-compile(export_all).

all() ->
    [file_not_exist,
     parse_error,
     validation_error,
     verify_error,
     unbound,
     demo0,
     format].

parse(C, F) ->
    Filename = filename:join(?config(data_dir, C), F),
    russell:run(["prim", "verify", Filename]),
    russell_prim:parse(Filename).

run(C, F) ->
    Filename = filename:join(?config(data_dir, C), F),
    russell:run(["prim", "verify", Filename]).

file_not_exist(C) ->
    {error, {_, russell_prim, {file_not_exist, _}}} =
        parse(C, "file_not_exist.prim"),
    ok.

parse_error(C) ->
    {error, {1, russell_prim_parser, _}} =
        parse(C, "parse_error.prim"),
    ok.

validation_error(C) ->
    {error,
     [{1, russell_prim, {stmt_defined, a}},
      {5, russell_prim, {stmt_defined, a}},
      {8, russell_prim, {stmt_not_found, a}}
     ]} =
        parse(C, "validation_error.prim"),
    ok.

verify_error(C) ->
    {error,
     [{1,russell_core,{def_not_found,p}},
      {9,russell_core,{def_not_found,q}},
      {16,russell_core,{input_number_mismatch,1,2}},
      {23,russell_core,{input_number_mismatch,2,1}},
      {32,russell_core,{not_match,_,_}},
      {49,russell_core,{not_match,_,_}},
      {58,russell_core,{not_match,_,_,_,_}},
      {60,russell_prim,{duplicate_def,p1}}
     ]} =
        run(C, "verify_error.prim"),
    ok.

unbound(C) ->
    ok = run(C, "unbound.prim"),
    ok.

demo0(_) ->
    ok =
        russell_prim_verify:run(
          [filename:join(code:lib_dir(russell, priv), "demo0.prim")]),
    ok.

format(_) ->
    Filename = filename:join(code:lib_dir(russell, priv), "demo0.prim"),
    {ok, Forms} = russell_prim:parse(Filename),
    {ok, Bin} = file:read_file(Filename),
    Bin = iolist_to_binary(russell_prim:format(Forms)),
    ok.
