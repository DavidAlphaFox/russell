PROOFS=$(wildcard pm/*.pf)

all: th1 $(PROOFS:%.pf=%)

th1:
	./bin/russell demo0.def th1.pf

pm/%: pm/%.pf
	./bin/russell pm.def $<
