SRC_DIR=$(CURDIR)/src
TEST_DIR=$(CURDIR)/tests
BUILD_SUPPORT=$(CURDIR)/build-support
# =============================================================================
# Verify that the programs we need to run are installed on this system
# =============================================================================
EMACS = $(shell which emacs)

ifeq ($(EMACS),)
$(error "Emacs not available on this system")
endif

.PHONY: all lint test clean

all: lint test

lint:
	$(EMACS) -Q --batch -L $(SRC_DIR) -l $(BUILD_SUPPORT)/elisp-lint.el \
	-f elisp-lint-files-batch --no-package-format $(SRC_DIR)/*.el

test:
	$(EMACS) -batch -l ert -L $(SRC_DIR) -L $(TEST_DIR) \
	-f ert-run-tests-batch-and-exit

clean:
	rm -f $(SRC_DIR)/*.elc
	rm -f $(TEST_DIR)/*.elc
