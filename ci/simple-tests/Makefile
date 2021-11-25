# This file is part of Proof General.
# 
# © Copyright 2021  Hendrik Tews
# 
# Authors: Hendrik Tews
# Maintainer: Hendrik Tews <hendrik@askra.de>
# 
# SPDX-License-Identifier: GPL-3.0-or-later


TESTS:=$(wildcard test-*.el)
SUCCESS:=$(TESTS:.el=.success)

all: $(SUCCESS)

test-%.success: test-%.el
	emacs -batch -l ../../generic/proof-site.el -l $< \
		-f ert-run-tests-batch-and-exit \
	&& touch $@

.PHONY: clean
clean:
	rm -f *.vo *.glob *.vio *.vos *.vok .*.aux *.success
