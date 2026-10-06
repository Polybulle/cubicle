# Review additions. Frozen probe.mk and probe.ml remain unchanged.
ISOQA_REVIEW = tests/state-dispatch-scheduler
.PHONY: isoqa-review-approx
isoqa-review-approx: $(CMX)
	mkdir -p $(ISOQA_REVIEW)/.local
	$(OCAMLOPT) $(OFLAGS) -I . -c -o $(ISOQA_REVIEW)/.local/review_approx.cmx $(ISOQA_REVIEW)/review-approx.ml
	$(OCAMLOPT) $(OFLAGS) -I . -o $(ISOQA_REVIEW)/.local/review-approx.opt $(BIBOPT) $(filter-out main.cmx,$(CMX)) $(ISOQA_REVIEW)/.local/review_approx.cmx
