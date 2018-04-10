PFS_FILES=$(wildcard *.pfs pm/*.pfs)
PROOFS=$(PFS_FILES:%.pfs=%.pf)

all: $(PROOFS)

pm/%.pf: pm/%.pfs
	./bin/russell pfs pm.def $< $@
	./bin/russell pf pm.def $@

%.pf: %.pfs
	./bin/russell pfs demo0.def $< $@
	./bin/russell pf demo0.def $@

clean:
	rm -f $(PROOFS)
