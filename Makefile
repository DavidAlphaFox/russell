.PRECIOUS: priv/%.prim
PRIM_FILES=$(wildcard priv/*.prim)
DEM_FILES=$(wildcard priv/*.dem)
PRIMS=$(PRIM_FILES:priv/%.prim=%.prim) $(DEM_FILES:priv/%.dem=%.prim)

all: $(PRIMS)

%.prim: priv/%.prim
	./bin/russell prim verify "$<"

priv/%.prim: priv/%.dem
	./bin/russell dem "$<" "$@"

clean:
	rm -f $(DEM_FILES:priv/%.dem=priv/%.prim)
