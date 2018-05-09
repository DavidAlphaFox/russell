-module(prim_compile_SUITE).

-include_lib("common_test/include/ct.hrl").
-compile(export_all).

all() ->
    [file_not_exist,
     def_not_found,
     demo0].

run(C, F) ->
    russell:run(["prim", "compile", filename:join(?config(data_dir, C), F)]).

file_not_exist(C) ->
    {error, {_, russell_prim, {file_not_exist, _}}} =
        run(C, "file_not_exist.prim"),
    ok.

def_not_found(C) ->
    {error, [{_, [_, {1,russell_prim_compile,{def_not_found,p}}]}]} =
        run(C, "def_not_found.prim").

demo0(_) ->
    ok =
        russell_prim:run(
          ["compile", filename:join(code:lib_dir(russell, priv), "demo0.prim")]),
    ok.
