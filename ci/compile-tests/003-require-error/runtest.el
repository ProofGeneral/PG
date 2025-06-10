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
;; Test parallel background compilation when coqdep fails on a require
;; command.
;;
;; The following graph shows the file dependencies in this test, where
;; X does not exist:
;;
;;   a1    a2   a3
;;   |    /  \ /
;;   b   X    c
;;
;; and where a1, a2 and a3 are the 3 require commands in file a.v

;;; Code:

;; require cct-lib for the elisp compilation, otherwise this is present already
(require 'cct-lib "ci/compile-tests/cct-lib")

;;; set configuration
(cct-configure-proof-general)

;;; Define the tests

(ert-deftest cct-require-error ()
  "Test background compilation with an require that yields a coqdep error."
  (let ((test-start-time (current-time)))
    (find-file "a.v")
    (cct-process-to-line 26)

    (cct-check-locked 22 'locked)
    (cct-check-locked 23 'unlocked)
    (cct-file-newer "b.vo" test-start-time)
    (cct-file-newer "c.vo" test-start-time)))

(provide 'runtest)

;;; runtest.el ends here
