;; This file is part of Proof General.
;; 
;; © Copyright 2022  Dominique Unruh
;; 
;; Authors: Dominique Unruh
;; Maintainer: Dominique Unruh
;; 
;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:
;;
;; Tests related to the qRHL prover
;;

(ert-deftest load-qrhl-input ()
  :expected-result :passed
  "Test that the qRHL input method loads without errors in .qrhl files"

  (message "load-qrhl-input test: Check loading of qRHL input method")
  (find-file "test.qrhl")
  (should (string= current-input-method "qrhl"))

  ;; To simulate typing, we put the keys into `unread-command-events'.
  ;; To process them normally, we enter a recursive edit. To abort the
  ;; recursive edit, we add \C-\M-c, the binding of
  ;; `exit-recursive-edit' to the simulated keys.
  (setq unread-command-events (listify-key-sequence "*\\subC\C-\M-c"))
  (recursive-edit)
  (should (equal "*⇩C" (buffer-substring 1 (point)))))
