# This file is part of Proof General.
# 
# © Copyright 2020  Hendrik Tews
# 
# Authors: Hendrik Tews
# Maintainer: Hendrik Tews <hendrik@askra.de>
# 
# SPDX-License-Identifier: GPL-3.0-or-later


# This test modifies some .v files during the test. The original
# versions are in .v.orig files. They are moved to the corresponding
# .v files before the test starts.
TEST_SOURCES:=a.v b.v c.v d.v e.v f.v g.v h.v i.v j.v k.v

.PHONY: test
test:
	$(MAKE) clean
	$(MAKE) $(TEST_SOURCES)
	emacs -batch -l ../../../generic/proof-site.el -l ../cct-lib.el \
		-l runtest.el -f ert-run-tests-batch-and-exit

%.v: %.v.orig
	cp $< $@

.PHONY: clean
clean:
	rm -f *.vo *.glob *.vio *.vos *.vok .*.aux $(TEST_SOURCES)
