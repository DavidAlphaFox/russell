-module(dem_SUITE).

-include_lib("common_test/include/ct.hrl").
-compile(export_all).

all() ->
    [file_not_exist,
     prim_error,
     subst_defined,
     subst_unbound,
     num_not_found,
     def_not_found,
     alias_not_found,
     unification,
     pm].

run(C, F) ->
    Filename = filename:join(?config(data_dir, C), F),
    russell:run(["dem", Filename]).

file_not_exist(C) ->
    {error, {_, russell_dem, {file_not_exist, _}}} =
        run(C, "file_not_exist.dem"),
    ok.

prim_error(C) ->
    {error,{2,russell_prim,_}} =
        run(C, "prim_error.dem"),
    ok.

subst_defined(C) ->
    {error,{4,russell_dem,{subst_defined,p}}} =
        run(C, "subst_defined.dem"),
    ok.

subst_unbound(C) ->
    {error,{6,russell_dem,{unbound_var_found,p}}} =
        run(C, "subst_unbound.dem"),
    ok.

num_not_found(C) ->
    {error,{4,russell_dem,{num_not_found,'1'}}} =
        run(C, "num_not_found.dem"),
    ok.

def_not_found(C) ->
    {error,{5,russell_dem,{def_not_found,r}}} =
        run(C, "def_not_found.dem"),
    ok.

alias_not_found(C) ->
    {error,{1,russell_dem,{alias_not_found,p}}} =
        run(C, "alias_not_found.dem"),
    ok.

unification(C) ->
    {error,{4,russell_dem,unification}} =
        run(C, "unification.dem"),
    ok.

pm(_) ->
    ok =
        russell_dem:run(
          [filename:join(code:lib_dir(russell, priv), "pm.dem")]),
    ok.
