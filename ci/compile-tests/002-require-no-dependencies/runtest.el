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
;; Test parallel background compilation when one require command does
;; not produce any dependencies.
;;
;; The following graph shows the file dependencies in this test:
;; 
;;      a    b
;;           |
;;           c

;;; Code:

;; require cct-lib for the elisp compilation, otherwise this is present already
(require 'cct-lib "ci/compile-tests/cct-lib")

;;; set configuration
(cct-configure-proof-general)

;;; Define the tests

(ert-deftest cct-one-require-with-no-dependencies ()
  "Test background compilation with an require with no dependencies."
  (find-file "a.v")
  (cct-process-to-line 25)
  
  (cct-check-locked 24 'locked)
  (cct-locked-ancestors 22 '()))
    
(ert-deftest cct-two-requires-first-no-dependencies ()
  "Test background compilation with an require with no dependencies."
  (find-file "b.v")
  (cct-process-to-line 25)
  
  (cct-check-locked 24 'locked)
  (cct-locked-ancestors 22 '())
  (cct-locked-ancestors 23 '("./c.v")))

(provide 'runtest)

;;; runtest.el ends here
