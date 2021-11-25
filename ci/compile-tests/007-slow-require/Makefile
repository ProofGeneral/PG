# This file is part of Proof General.
# 
# © Copyright 2021  Hendrik Tews
# 
# Authors: Hendrik Tews
# Maintainer: Hendrik Tews <hendrik@askra.de>
# 
# SPDX-License-Identifier: GPL-3.0-or-later


# This test modifies some .v files during the test. The original
# versions are in .v.orig files. They are moved to the corresponding
# .v files before the test starts.
TEST_SOURCES:=\
	a1.v b1.v c1.v d1.v \
	a2.v b2.v c2.v d2.v \
	a3.v b3.v c3.v d3.v \
	a4.v b4.v c4.v d4.v \
	a5.v b5.v c5.v d5.v \
	a6.v b6.v c6.v d6.v

# This test uses ../bin/compile-test-start-delayed to start certain
# commands with specified delays to check carfully constructed
# internal states. compile-test-start-delayed outputs diagnostics on
# file descriptor 9, which bypasses emacs and is joined with stderr of
# the current make. Open file descriptor 9 here.
.PHONY: test
test:
	$(MAKE) clean
	$(MAKE) $(TEST_SOURCES)
	emacs -batch -l ../../../generic/proof-site.el -l ../cct-lib.el \
		-l runtest.el -f ert-run-tests-batch-and-exit \
		9>&1

%.v: %.v.orig
	cp $< $@

.PHONY: clean
clean:
	rm -f *.vo *.glob *.vio *.vos *.vok .*.aux $(TEST_SOURCES)
