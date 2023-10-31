# This file is part of Proof General.
# 
# © Copyright 2021  Hendrik Tews
# 
# Authors: Hendrik Tews
# Maintainer: Hendrik Tews <hendrik@askra.de>
# 
# SPDX-License-Identifier: GPL-3.0-or-later


COQ_TESTS:=$(wildcard coq-test-*.el)
QRHL_TESTS:=$(wildcard qrhl-test-*.el)

COQ_SUCCESS:=$(COQ_TESTS:.el=.success)
QRHL_SUCCESS:=$(QRHL_TESTS:.el=.success)

.PHONY: coq-all
coq-all: $(COQ_SUCCESS)

.PHONY: qrhl-all
qrhl-all: $(QRHL_SUCCESS)

%.success: %.el
	emacs -batch -l ../../generic/proof-site.el -l $< \
		-f ert-run-tests-batch-and-exit \
	&& touch $@

.PHONY: clean
clean:
	rm -f *.vo *.glob *.vio *.vos *.vok .*.aux *.success
