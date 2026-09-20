TEST_DIR = tests/forward-transactions
TEST_LOCAL = $(TEST_DIR)/.local
.PHONY: forward-transactions-check
forward-transactions-check: $(CMX)
	mkdir -p $(TEST_LOCAL)
	$(OCAMLOPT) $(OFLAGS) -I . -c -o $(TEST_LOCAL)/check.cmx $(TEST_DIR)/check.ml
	$(OCAMLOPT) $(OFLAGS) -I . -o $(TEST_LOCAL)/check.opt $(BIBOPT) $(filter-out main.cmx,$(CMX)) $(TEST_LOCAL)/check.cmx
