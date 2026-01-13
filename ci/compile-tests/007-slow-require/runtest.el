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
;; Test two require jobs with different delays such that
;; coq-par-retire-top-level-job happens when the other require job is
;; in each possible state.  For specifying the different delays there
;; are 6 mini projects, one for each test in this file.  Each project
;; consists of four files, a1.v, b1.v, c1.v and d1.v for the first one
;; and a6.v, b6.v, c6.v and d6.v for the sixth one.  Each project is
;; one ert test, described further below.  In each project or test, the
;; top level require commands are asserted and retracted several times
;; with changes in different files to test (almost) all possible
;; internal state combinations.
;;
;; The dependencies are the same in all projects:
;; 
;;           a
;;          / \
;;         b   c
;;          \ /
;;           d
;;
;; Test 1 delays coqdep on require c, such that queue kickoff happens
;; in state 'enqueued-coqdep, the dependency link d->c is created when
;; d is ready and kickoff-queue-maybe on require c happens when
;; require b is in state 'ready.
;;
;; Test 2 delays coqdep on c, such that queue kickoff happens in state
;; 'waiting-dep.
;;
;; Test 3 delays coqdep on require b, such that kickoff-queue-maybe on
;; require c happens when require b is in state 'enqueued-coqdep and
;; queue kickoff happens in state 'waiting-queue.
;;
;; Test 4 delays coqdep on b, such that kickoff-queue-maybe on require
;; c happens when require b is in state 'waiting-dep.
;;
;; Test 5 delays coqdep on d, such that the second dependency on d is
;; created when d is in state enqueued-coqdep.
;;
;; Test 6 delays coqdep on c for 2 seconds and coqc on d for 4
;; seconds, such that the dependency link d->c is created when d is in
;; state enqueued-coqc.

;;; Code:

;; require cct-lib for the elisp compilation, otherwise this is present already
(require 'cct-lib "ci/compile-tests/cct-lib")

;;; set configuration
(cct-configure-proof-general)
(configure-delayed-coq)

(defconst pre-b-ancestors '("./b" "./d")
    "Ancestors of b without suffixes.")

(defconst pre-c-ancestors '("./c")
  "Ancestors of c without suffixes.")

(defconst pre-all-ancestors (append pre-b-ancestors pre-c-ancestors)
  "All ancestors without suffixes.")

(defun b-ancestors (n)
  "Ancestors of b for part N."
  (mapcar (lambda (a) (concat a (format "%d.v" n))) pre-b-ancestors))

(defun c-ancestors (n)
  "Ancestors of c for part N."
  (mapcar (lambda (a) (concat a (format "%d.v" n))) pre-c-ancestors))

(defun all-ancestors (n)
  "All ancestors for part N."
  (mapcar (lambda (a) (concat a (format "%d.v" n))) pre-all-ancestors))

(defun all-compiled-ancestors (n)
  "All vo ancestor files for part N."
  (mapcar #'cct-library-vo-of-v-file (all-ancestors n)))

(defun check-main-buffer (n vo-times new-sum recompiled-files
                                       other-locked-files)
  "Perform various checks in buffer aN.v.
See `cct-generic-check-main-buffer'."
  (cct-generic-check-main-buffer
   (format "a%d.v" n)                   ; main-buf
   25                                   ; main-unlocked
   41                                   ; main-locked
   31                                   ; main-sum-line
   new-sum
   vo-times
   recompiled-files
   `((25 . ,(b-ancestors n)) (26 . ,(c-ancestors n))) ; require-ancestors
   other-locked-files
   27                                   ; other-locked-line
   ))


;;; Define the test

(defun test-slow-require (n)
  "Test part N of slow require job tests.
XXX Test a setting where the second require job is still in state
'enqueued-coqdep when the first one finishes."
  (let*
      ;; .v file names are used as file and as buffer names
      ((av (format "a%d.v" n))
       (bv (format "b%d.v" n))
       (cv (format "c%d.v" n))
       (dv (format "d%d.v" n))
       ;; .vo names compared with ancestors, they need a "./" prefix
       (bvo (concat "./" (cct-library-vo-of-v-file bv)))
       (cvo (concat "./" (cct-library-vo-of-v-file cv)))
       (dvo (concat "./" (cct-library-vo-of-v-file dv)))
       vo-times other-locked-files)

    ;; (setq cct--debug-tests t)
    ;; (setq coq--debug-auto-compilation t)
    (find-file av)
    (message "coqdep: %s\ncoqc: %s\nPATH %s\nexec-path: %s"
             coq-dependency-analyzer
             coq-compiler
             (getenv "PATH")
             exec-path)

    ;; 1. process original content - compile everything
    (message "\n%d.1. process original content - compile everything\n" n)
    ;;(setq coq--debug-auto-compilation t)
    (cct-process-to-line 42)
    (cct-check-locked 41 'locked)
    (cct-locked-ancestors 25 (b-ancestors n))
    (cct-locked-ancestors 26 (c-ancestors n))

    ;; (cl-assert nil nil "exit")

    (setq vo-times (cct-record-change-times (all-compiled-ancestors n)))
    ;; 2. retract and process again - nothing should be compiled
    (message "\n%d.2. retract and process again - nothing should be compiled\n"
             n)
    (cct-process-to-line 24)
    (check-main-buffer n vo-times "9" nil nil)

    ;; 3. change d and process again - b and c should be compiled
    (message "\n%d.3. change d and process again - b and c should be compiled\n"
             n)
    (find-file dv)
    (push dv other-locked-files)
    (cct-check-locked 23 'locked)
    (cct-replace-last-word 27 "5")
    (check-main-buffer n vo-times "10" (list bvo cvo dvo)
                       other-locked-files)

    ;; 4. change b and process again - only b should be compiled
    (message "\n%d.4. change b and process again - only b should be compiled\n"
             n)
    (setq vo-times (cct-record-change-times (all-compiled-ancestors n)))
    (find-file bv)
    (push bv other-locked-files)
    (cct-check-locked 23 'locked)
    (cct-replace-last-word 27 "4")
    (check-main-buffer n vo-times "12" (list bvo) other-locked-files)

    ;; 5. change c and process again - only c should be compiled
    (message "\n%d.5. change c and process again - only c should be compiled\n"
             n)
    (setq vo-times (cct-record-change-times (all-compiled-ancestors n)))
    (find-file cv)
    (push cv other-locked-files)
    (cct-check-locked 23 'locked)
    (cct-replace-last-word 27 "8")
    (check-main-buffer n vo-times "17" (list cvo) other-locked-files)

    ;; 6. delete d and process again - b and c should be compiled
    (message "\n%d.6. delete d and process again - b and c should be compiled\n"
             n)
    (setq vo-times (cct-record-change-times (all-compiled-ancestors n)))
    (delete-file dvo)
    (set-buffer av)
    (cct-process-to-line 24)
    (check-main-buffer n vo-times "17" (list bvo cvo dvo)
                       other-locked-files)

    ;; 7. delete b and process again - only b should be compiled
    (message "\n%d.7. delete b and process again - only b should be compiled\n"
             n)
    (setq vo-times (cct-record-change-times (all-compiled-ancestors n)))
    (delete-file bvo)
    (set-buffer av)
    (cct-process-to-line 24)
    (check-main-buffer n vo-times "17" (list bvo) other-locked-files)

    ;; 8. delete c and process again - only c should be compiled
    (message "\n%d.8. delete c and process again - only c should be compiled\n"
             n)
    (setq vo-times (cct-record-change-times (all-compiled-ancestors n)))
    (delete-file cvo)
    (set-buffer av)
    (cct-process-to-line 24)
    (check-main-buffer n vo-times "17" (list cvo) other-locked-files)
  ))


(ert-deftest cct-slow-require-1 () (test-slow-require 1))
(ert-deftest cct-slow-require-2 () (test-slow-require 2))
(ert-deftest cct-slow-require-3 () (test-slow-require 3))
(ert-deftest cct-slow-require-4 () (test-slow-require 4))
(ert-deftest cct-slow-require-5 () (test-slow-require 5))
(ert-deftest cct-slow-require-6 () (test-slow-require 6))

(provide 'runtest)

;;; runtest.el ends here
