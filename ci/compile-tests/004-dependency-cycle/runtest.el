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
;; Test parallel background compilation for a dependency cycle.
;;
;; The following graph shows the file dependencies in this test:
;;
;;              a1   a2
;;             /     |
;;            b<-e   f
;;            |  |
;;            c->d
;;
;; where a1 and a2 are the 2 require commands in file a.v.

;;; Code:

;; require cct-lib for the elisp compilation, otherwise this is present already
(require 'cct-lib "ci/compile-tests/cct-lib")

;;; set configuration
(cct-configure-proof-general)

;;; Define the tests

(ert-deftest cct-require-error ()
  "Test background compilation on cyclic dependencies."
  (find-file "a.v")
  (cct-process-to-line 25)
  (let ((last-message (cct-last-message-line))
        message-cycle)
    (should (equal "Coq compilation error: Circular dependency"
                   (substring last-message 0 42)))

    (should
     (string-match
      (concat
       "\\./\\([b-e]\\).v -> \\./\\([b-e]\\).v -> "
       "\\./\\([b-e]\\).v -> \\./\\([b-e]\\).v")
      last-message))
    (setq message-cycle
          (list (match-string 1 last-message) (match-string 2 last-message)
                (match-string 3 last-message) (match-string 4 last-message)))
    (should
     (or (equal message-cycle '("b" "c" "d" "e"))
         (equal message-cycle '("c" "d" "e" "b"))
         (equal message-cycle '("d" "e" "b" "c"))
         (equal message-cycle '("e" "b" "c" "d"))))

    (cct-check-locked 21 'locked)
    (cct-check-locked 22 'unlocked)))

(provide 'runtest)

;;; runtest.el ends here
