;;; runtest.el --- Test -*- lexical-binding: t; -*-
;;
;; This file is part of Proof General.
;;
;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:
;;
;; Coq Compile Tests (cct) --
;; ert tests for parallel background compilation for Coq
;;
;; Test that the user can interrupt a background compilation while
;; Coq itself is idle.  Proof General must then kill the compilation
;; and clear the queue region instead of complaining that the proof
;; process is not active.  This relies on `coq-par-user-interrupt'
;; setting `prover-was-busy', which `proof-interrupt-process' binds
;; dynamically.

;;; Code:

(require 'cct-lib "ci/compile-tests/cct-lib")

(cct-configure-proof-general)
(configure-delayed-coq)

(defvar coq--last-compilation-job)
(declare-function proof-goto-point "pg-user")
(declare-function proof-interrupt-process "proof-shell")
(declare-function proof-unprocessed-begin "proof-script")

(ert-deftest cct-interrupt-compilation ()
  "Interrupt the background compilation of b.v while Coq is idle."
  (find-file "a.v")
  (cct-goto-line 16)
  (proof-goto-point)
  ;; Wait until the compilation of b.v runs.  coqc is delayed by 5
  ;; seconds (see b.v), therefore it is still running after that.
  (let ((start (float-time)))
    (while (and (not (and coq--last-compilation-job
                          (> (float-time) (+ start 1))))
                (< (float-time) (+ start 10)))
      (accept-process-output nil 0.1)))
  ;; The Require waits for the compilation, nothing is sent to Coq.
  (should coq--last-compilation-job)
  (should-not proof-action-list)

  ;; `coq-par-user-interrupt' releases the proof shell lock, therefore
  ;; `proof-interrupt-process' relies on `prover-was-busy' and must not
  ;; signal "Proof process not active!".
  (proof-interrupt-process)

  (should-not coq--last-compilation-job)
  (should-not proof-shell-busy)
  (should-not proof-action-list)
  ;; the Require has not been processed
  (should (< (proof-unprocessed-begin) (progn (cct-goto-line 15) (point))))
  ;; the compilation has been killed and does not produce b.vo
  (sleep-for 6)
  (should-not (file-exists-p "b.vo")))

(provide 'runtest)
