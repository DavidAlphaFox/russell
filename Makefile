.PRECIOUS: priv/%.prim
PRIM_FILES=$(wildcard priv/*.prim)
DEM_FILES=$(wildcard priv/*.dem)
BEAM_FILES=$(PRIM_FILES:priv/%.prim=priv/%.beam) $(DEM_FILES:priv/%.dem=priv/%.beam)

all: $(BEAM_FILES)

priv/%.beam: priv/%.prim
	./bin/russell prim verify "$<"
	./bin/russell prim compile "$<"
	escript "$@"

priv/%.prim: priv/%.dem
	./bin/russell dem "$<" "$@"

clean:
	rm -f $(DEM_FILES:priv/%.dem=priv/%.prim) $(BEAM_FILES)
