TEST_DIR = tests/cfg-boundaries
TEST_LOCAL = $(TEST_DIR)/.local
.PHONY: cfg-boundaries-check
cfg-boundaries-check: $(CMX)
	mkdir -p $(TEST_LOCAL)
	$(OCAMLOPT) $(OFLAGS) -I . -c -o $(TEST_LOCAL)/check.cmx $(TEST_DIR)/check.ml
	$(OCAMLOPT) $(OFLAGS) -I . -o $(TEST_LOCAL)/check.opt $(BIBOPT) $(filter-out main.cmx,$(CMX)) $(TEST_LOCAL)/check.cmx
	$(TEST_LOCAL)/check.opt -tx all tests/entrypoint-covering/model.cub
	$(TEST_LOCAL)/check.opt -tx none tests/entrypoint-covering/model.cub
