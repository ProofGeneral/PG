# This file is part of Proof General.
# 
# © Copyright 2024  Hendrik Tews
# 
# Authors: Hendrik Tews
# Maintainer: Hendrik Tews <hendrik@askra.de>
# 
# SPDX-License-Identifier: GPL-3.0-or-later

# This test uses ../bin/compile-test-start-delayed to start certain
# commands with specified delays to check carfully constructed
# internal states. compile-test-start-delayed outputs diagnostics on
# file descriptor 9, which bypasses emacs and is joined with stderr of
# the current make. Open file descriptor 9 here.
#
# To run not all tests, replace line
#
# 		-f ert-run-tests-batch-and-exit \
#
# with
#
#		-eval '(ert-run-tests-batch-and-exit "pattern")' \
#
# where pattern should match the test names to run.

.PHONY: test
test:
	$(MAKE) clean
	emacs -batch -l ../../../generic/proof-site.el -l ../cct-lib.el \
		-l runtest.el \
		-f ert-run-tests-batch-and-exit \
		9>&1

.PHONY: clean
clean:
	rm -f *.vo *.glob *.vio *.vos *.vok .*.aux
