TEST_DIR = tests/neutral-candidates
TEST_LOCAL = $(TEST_DIR)/.local
.PHONY: neutral-candidates-check
neutral-candidates-check: $(CMX)
	mkdir -p $(TEST_LOCAL)
	$(OCAMLOPT) $(OFLAGS) -I . -c -o $(TEST_LOCAL)/check.cmx $(TEST_DIR)/check.ml
	$(OCAMLOPT) $(OFLAGS) -I . -o $(TEST_LOCAL)/check.opt $(BIBOPT) $(filter-out main.cmx,$(CMX)) $(TEST_LOCAL)/check.cmx
