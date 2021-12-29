# This file is part of Proof General.
# 
# © Copyright 2021  Pierre Courtieu
# 
# Authors: Pierre Courtieu
# Maintainer: Pierre Courtieu <Pierre.Courtieu@cnam.fr>
# 
# SPDX-License-Identifier: GPL-3.0-or-later

TESTS:=$(wildcard indent-*.v)
INDENTED:=$(subst indent-,indented_indent-, $(TESTS))

all: $(INDENTED)

indented_%.v: %.v
	@echo "### testing indentation of $<..."
	@./coq-test-indent.sh $<

.PHONY: clean

clean:
	rm -f indented_indent*
