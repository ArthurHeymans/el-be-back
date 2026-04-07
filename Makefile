.PHONY: all build test test-elisp clean

EMACS ?= emacs

all: build test

build:
	./build.sh

test: build
	$(EMACS) -batch -L . -l test/ebb-test.el -f ert-run-tests-batch-and-exit

test-elisp:
	$(EMACS) -batch -L . -l el-be-back.el -l test/ebb-test.el \
		--eval "(ert-run-tests-batch-and-exit '(member ebb-test-raw-key-sequences ebb-test-filter-soft-wraps ebb-test-clean-copy-text ebb-test-resolve-cwd ebb-test-resolve-cwd-remote ebb-test-osc133-scanning ebb-test-osc51-eval ebb-test-osc-sequence-scanning ebb-test-url-detection ebb-test-file-detection ebb-test-keymap-exceptions ebb-test-input-coalesce-multi-byte ebb-test-face-hex-color))"

clean:
	cargo clean
	rm -f ebb-module.so el-be-back.elc
