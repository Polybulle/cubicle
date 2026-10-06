TEST_DIR = tests/entrypoint-covering
TEST_LOCAL = $(TEST_DIR)/.local
.PHONY: entrypoint-covering-check
entrypoint-covering-check: $(CMX)
	mkdir -p $(TEST_LOCAL)
	$(OCAMLOPT) $(OFLAGS) -I . -c -o $(TEST_LOCAL)/check.cmx $(TEST_DIR)/check.ml
	$(OCAMLOPT) $(OFLAGS) -I . -o $(TEST_LOCAL)/check.opt $(BIBOPT) $(filter-out main.cmx,$(CMX)) $(TEST_LOCAL)/check.cmx
