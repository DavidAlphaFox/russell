PFS_FILES=$(wildcard *.pfs pm/*.pfs)
PROOFS=$(PFS_FILES:%.pfs=%.pf)

all: $(PROOFS)

%.pf: %.pfs
	./bin/russell pfs demo0.def $< $@
	./bin/russell pf demo0.def $@

pm/%.pf: pm/%.pfs
	./bin/russell pfs pm.def $< $@
	./bin/russell pf pm.def $@

clean:
	rm -f $(PROOFS)
