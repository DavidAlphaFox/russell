PRIM_FILES=$(wildcard priv/*.prim)
PRIMS=$(PRIM_FILES:priv/%.prim=%.prim)

all: $(PRIMS)

%.prim: priv/%.prim
	./bin/russell prim "$<"
