TEST_DIR = tests/ordinary-transitions
TEST_LOCAL = $(TEST_DIR)/.local
.PHONY: ordinary-transitions-check
ordinary-transitions-check: $(CMX)
	mkdir -p $(TEST_LOCAL)
	$(OCAMLOPT) $(OFLAGS) -I . -c -o $(TEST_LOCAL)/check.cmx $(TEST_DIR)/check.ml
	$(OCAMLOPT) $(OFLAGS) -I . -o $(TEST_LOCAL)/check.opt $(BIBOPT) $(filter-out main.cmx,$(CMX)) $(TEST_LOCAL)/check.cmx
	$(TEST_LOCAL)/check.opt -tx none $(TEST_DIR)/model.cub
	$(TEST_LOCAL)/check.opt -tx bwd $(TEST_DIR)/model.cub
	$(TEST_LOCAL)/check.opt -tx all $(TEST_DIR)/model.cub
