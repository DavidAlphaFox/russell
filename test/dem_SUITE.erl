-module(dem_SUITE).

-include_lib("common_test/include/ct.hrl").
-compile(export_all).

all() ->
    [pm].

pm(_) ->
    ok =
        russell_dem:run(
          [filename:join(code:lib_dir(russell, priv), "pm.dem")]),
    ok.
