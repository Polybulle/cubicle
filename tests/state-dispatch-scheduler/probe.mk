# Run from repository root after make. Production sources are not changed.
ISOQA_DIR = tests/state-dispatch-scheduler
ISOQA_LOCAL = $(ISOQA_DIR)/.local
.PHONY: isoqa-probe
isoqa-probe: $(CMX)
	mkdir -p $(ISOQA_LOCAL)
	$(OCAMLOPT) $(OFLAGS) -I . -c -o $(ISOQA_LOCAL)/probe.cmx $(ISOQA_DIR)/probe.ml
	$(OCAMLOPT) $(OFLAGS) -I . -o $(ISOQA_LOCAL)/probe.opt $(BIBOPT) $(filter-out main.cmx,$(CMX)) $(ISOQA_LOCAL)/probe.cmx
