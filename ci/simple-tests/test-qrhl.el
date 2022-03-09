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
  ;; Ideally we would do some simulated keypresses and check whether they
  ;; are translated correctly. But I don't know how. (Dominique)
  (should (string= current-input-method "qrhl"))
  )
