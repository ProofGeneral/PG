# This file is part of Proof General.
#
# SPDX-License-Identifier: GPL-3.0-or-later


.PHONY: test
test:
	$(MAKE) clean
	emacs -batch -l ../../../generic/proof-site.el -l ../cct-lib.el \
		-l runtest.el -f ert-run-tests-batch-and-exit \
		9>&1

.PHONY: clean
clean:
	rm -f *.vo *.glob *.vio *.vos *.vok .*.aux
