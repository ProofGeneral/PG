;;; runtest.el --- Test -*- lexical-binding: t; -*-
;;
;; This file is part of Proof General.
;; 
;; © Copyright 2020  Hendrik Tews
;; 
;; Authors: Hendrik Tews
;; Maintainer: Hendrik Tews <hendrik@askra.de>
;; 
;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:
;;
;; Coq Compile Tests (cct) --
;; ert tests for parallel background compilation for Coq
;;
;; Test that parallel background compilation works for a simple
;; project and that the right files are recorded for unlocking at the
;; right places.
;;
;; The following graph shows the file dependencies in this test:
;; 
;;           a
;;          / \
;;         b   c
;;        / \ / \
;;       d   e   f

;;; Code:

;; require cct-lib for the elisp compilation, otherwise this is present already
(require 'cct-lib "ci/compile-tests/cct-lib")

;;; set configuration
(cct-configure-proof-general)

;;; Define the test

(ert-deftest cct-mini-project ()
  "Test successful background compilation and ancestor recording."
  (find-file "a.v")
  ; (setq coq--debug-auto-compilation t)
  (cct-process-to-line 25)
  
  (cct-check-locked 24 'locked)
  (cct-locked-ancestors 22 '("./b.v" "./d.v" "./e.v"))
  (cct-locked-ancestors 23 '("./c.v" "./f.v")))

(provide 'runtest)

;;; runtest.el ends here
