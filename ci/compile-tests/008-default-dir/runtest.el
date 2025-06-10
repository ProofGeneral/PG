;;; runtest.el --- Test -*- lexical-binding: t; -*-
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
;; Coq Compile Tests (cct) --
;; ert tests for parallel background compilation for Coq
;;
;; Test that default-directory is correctly set independently of the
;; current buffer in the foreground, both for first and second stage
;; compilation.
;;
;; This test fails for Coq 8.15.0, because the test checks .vok files,
;; which, in certain situations, are not created by that coq version.
;; See Coq issue 15773.
;;
;; The dependencies in this test are:
;; 
;;           a
;;           |
;;           b
;;           |
;;           c
;;

;;; Code:

;; require cct-lib for the elisp compilation, otherwise this is present already
(require 'cct-lib "ci/compile-tests/cct-lib")

;;; set configuration
(cct-configure-proof-general)
(configure-delayed-coq)

(defconst all-ancestors '("./b.v" "./c.v")
  "All ancestors.")

(defconst all-compiled-ancestors
  (mapcar #'cct-library-vo-of-v-file all-ancestors)
  "All vo ancestors files.")

(defun check-main-buffer (vo-times new-sum recompiled-files
                                       other-locked-files)
  "Perform various checks in buffer a.v.
See `cct-generic-check-main-buffer'."
  (cct-generic-check-main-buffer
   "a.v"                                ; main-buf
   24                                   ; main-unlocked
   40                                   ; main-locked
   30                                   ; main-sum-line
   new-sum
   vo-times
   recompiled-files
   `((25 . ,all-ancestors))             ; require-ancestors
   other-locked-files
   23                                   ; other-locked-line
   ))


;;; The test itself

(ert-deftest test-default-dir ()
  :expected-result (if (equal (coq-version t) "8.15.0") :failed  :passed)
  "`default-directory' test.
Test that `default-directory' is correctly set independently of the
current buffer in the foreground.

This test fails for Coq 8.15.0, because the test checks .vok
files, which are not created by that coq version, in certain
situations. See Coq issue 15773."
  (let (vo-times av-buffer ci-buffer other-locked-files
        vok-times vos-vio-files)

    ;; (setq cct--debug-tests t)
    ;; (setq coq--debug-auto-compilation t)
    (find-file "a.v")
    (setq av-buffer (current-buffer))

    (message
     "coqdep: %s\ncoqc: %s\nPATH %s\nexec-path: %s\ndetected coq version: %s"
     coq-dependency-analyzer
     coq-compiler
     (getenv "PATH")
     exec-path
     coq-autodetected-version)

    (find-file "../..")
    (setq ci-buffer (current-buffer))

    (add-hook 'cct-before-busy-waiting-hook (lambda () (set-buffer ci-buffer)))
    (add-hook 'cct-after-busy-waiting-hook (lambda () (set-buffer av-buffer)))

    ;; 1. process original content - compile everything
    (message "\n1. process original content - compile everything\n")
    (check-main-buffer vo-times "5" nil other-locked-files)
    (setq vo-times (cct-record-change-times all-compiled-ancestors))

    ;; 2. retract and process again - nothing should be compiled
    (message "\n2. retract and process again - nothing should be compiled\n")
    (cct-process-to-line 21)
    (check-main-buffer vo-times "5" nil other-locked-files)

    ;; 3. change b and process again - only b should get compiled
    (message "\n3. change b and process again - only b should get compiled\n")
    (find-file "b.v")
    (push "b.v" other-locked-files)
    (cct-check-locked 23 'locked)
    (cct-replace-last-word 23 "3")
    (check-main-buffer vo-times "6" '("./b.vo") other-locked-files)
    (setq vo-times (cct-record-change-times all-compiled-ancestors))

    ;; 4. change c and process again - b and c should get compiled
    (message "\n4. change c and process again - b and c should get compiled\n")
    (find-file "c.v")
    (push "c.v" other-locked-files)
    (cct-check-locked 23 'locked)
    (cct-replace-last-word 23 "5")
    (check-main-buffer vo-times "8" '("./b.vo" "./c.vo") other-locked-files)
    (setq vo-times (cct-record-change-times all-compiled-ancestors))

    ;; 5. change b and c and reprocess with vos-and-vok/quick-and-vio2vo
    (message
     "\n5. change b and c and reprocess with vos-and-vok/quick-and-vio2vo\n")
    (if (coq--post-v811)
        (setq coq-compile-vos 'vos-and-vok
              vos-vio-files '("./b.vos" "./c.vos")
              vok-times (cct-record-change-times '("./b.vok" "./c.vok")))
      (setq coq-compile-quick 'quick-and-vio2vo
            vos-vio-files '("./b.vio" "./c.vio")))

    (set-buffer "b.v")
    (cct-check-locked 23 'locked)
    (cct-replace-last-word 23 "7")
    (set-buffer "c.v")
    (cct-check-locked 23 'unlocked)
    (cct-replace-last-word 23 "10")
    (check-main-buffer nil "17" nil other-locked-files)

    (cct-files-are-readable vos-vio-files)
    ;; this will switch to a different default-directory, see the
    ;; hooks above
    (cct-wait-for-second-stage)
    (if (coq--post-v811)
        (progn
          (cct-older-change-times vok-times)
          (cct-unmodified-change-times vo-times))
      (cct-older-change-times vo-times))
    ))

(provide 'runtest)

;;; runtest.el ends here
