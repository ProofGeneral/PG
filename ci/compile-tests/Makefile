# This file is part of Proof General.
# 
# © Copyright 2020  Hendrik Tews
# 
# Authors: Hendrik Tews
# Maintainer: Hendrik Tews <hendrik@askra.de>
# 
# SPDX-License-Identifier: GPL-3.0-or-later


TOPTARGETS := test clean

SUBDIRS := $(wildcard [0-9][0-9][0-9]-*)

$(TOPTARGETS): $(SUBDIRS)
$(SUBDIRS):
	$(MAKE) -C $@ $(MAKECMDGOALS)

.PHONY: $(TOPTARGETS) $(SUBDIRS)

.PHONY: TAGS
TAGS:
	etags *.el */*.el
