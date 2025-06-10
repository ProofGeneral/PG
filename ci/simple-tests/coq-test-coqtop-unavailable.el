;;; coq-test-coqtop-unavailable.el --- Test for when coqtop is unavailable
;;
;; This file is part of Proof General.
;; 
;; © Copyright 2021  Hendrik Tews
;; 
;; Authors: Hendrik Tews
;; Maintainer: Hendrik Tews <hendrik@askra.de>
;; 
;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:
;;
;; Test that Proof General can open Coq files even when coqtop is
;; unavailable.


;;; Code:

(ert-deftest coqtop-unavailable ()
  "Proof General can open Coq files even when coqtop is unavailable.."
  (setq coq-prog-name "unavailable-program")

  ;; ensure coq-prog-name cannot be found
  (should (not (locate-file coq-prog-name exec-path)))

  (find-file "simple.v")
  (coq-prog-args))

(provide 'coq-test-coqtop-unavailable)

;;; coq-test-coqtop-unavailable.el ends here
