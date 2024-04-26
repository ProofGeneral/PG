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
coq-all: $(COQ_SUCCESS) coq-proof-stat-batch-test

.PHONY: qrhl-all
qrhl-all: $(QRHL_SUCCESS)

%.success: %.el
	emacs -batch -l ../../generic/proof-site.el -l $< \
		-f ert-run-tests-batch-and-exit \
	&& touch $@

# The following tests the batch mode functionality of
# proof-check-proofs. It generates human readable and TAP output for
# proof_stat.v and compares it with the expected output in file
# proof_stat.exp-out.
coq-proof-stat-batch-test:
	emacs -batch -l ../../generic/proof-site.el proof_stat.v \
	      --eval '(proof-check-proofs nil "proof_stat.human.gen-out")'
	emacs -batch -l ../../generic/proof-site.el proof_stat.v \
	      --eval '(proof-check-proofs t "proof_stat.tap.gen-out")'
	cmp proof_stat.human.gen-out proof_stat.human.exp-out && \
	        cmp proof_stat.tap.gen-out proof_stat.tap.exp-out && \
		touch coq-proof-stat-batch-test

.PHONY: clean
clean:
	rm -f *.vo *.glob *.vio *.vos *.vok .*.aux *.success
	rm -f coq-proof-stat-batch-test proof_stat.gen-out
