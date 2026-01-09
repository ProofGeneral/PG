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
;; Test a partially successful and partially failing compilation with
;; coq-compile-keep-going.  Check that the right files are compiled,
;; locked and unlocked.  Check also the case, where unlocking of failed
;; files must be delayed, because some earlier successful require job
;; has not yet locked its ancestors.
;;
;; The dependencies in this test are:
;;
;;    Rb    Rc  Rd
;;    |     |   |
;;    |     c   |
;;    |    /|   |
;;    |   / |   |
;;    |  e  |   d
;;    | ?   |   |
;;    |?    |   |
;;    b     g   /
;;    |   _/|  /
;;    | _/  | /
;;    |/    |/
;;    f     h
;;
;; Rb, Rc and Rd are three different require commands in file a. The
;; dependency e -> b is not present in test 5 and test 10 (but in all
;; other tests).  The error always happens in file g, for test 1-5 with
;; coqdep, for tests 6-10 with coqc.  There are 10 tests, each with
;; slighly different delays, in 10 versions of the sources, e.g.,
;; a1-a10, b1-b10, and so on.
;;
;; For tests 1-5 coqdep fails on g, when this happens
;;
;; 1: Rb is ready and d still busy
;; 2: f is ready and b and d are still busy and d finishes last
;; 3: f is ready and b and d are still busy and b finishes last
;; 4: RB is ready and Rd is queue waiting
;; 5: without dependency e -> b: Rc, Rd are queue waiting, b finishes last
;;
;;
;; For tests 6-10 coqc fails on g, when this happens
;;
;; 6: Rb is ready and d still busy
;; 7: f is ready and b and d are still busy and d finishes last
;; 8: f is ready and b and d are still busy and b finishes last
;; 9: RB is ready and Rd is queue waiting
;; 10: without dependency b -> e: Rc, Rd are queue waiting, b finishes last

;;; Code:

;; require cct-lib for the elisp compilation, otherwise this is present already
(require 'cct-lib "ci/compile-tests/cct-lib")

;;; set configuration
(cct-configure-proof-general)
(configure-delayed-coq)

(defconst pre-b-ancestors '("b" "f")
    "Ancestors of b without suffixes.")

(defconst pre-all-compiled (append pre-b-ancestors '("e" "h" "d"))
  "All files that get compiled.")

(defun pre-not-compiled (n)
  "List of file name stems for which coqc must not be called.
Files for which coqc must not be called have an ``X'' in
coqc-delay.  For such files `compile-test-start-delayed' would
create a ``.X'' file, whose absence is checked in the test."
  (cond
   ((< n 6) '("g" "c"))
   (t       '("c"))))

(defconst pre-all-unlocked '("c" "d" "e" "g" "h")
  "All stems of files that should be unlocked after compilation.")

(defun b-ancestors (n)
  "Ancestors of b for part N."
  (mapcar (lambda (f) (format "./%s%d.v" f n)) pre-b-ancestors))

(defun all-compiled (n)
  "All files that get compiled for part N."
  (mapcar (lambda (f) (format "./%s%d.v" f n)) pre-all-compiled))

(defun all-compiled-vo (n)
  "All vo files for part N."
  (mapcar #'cct-library-vo-of-v-file (all-compiled n)))

(defun not-compiled (n)
  "Files that should not be compiled for part N, see `pre-not-compiled'."
  (mapcar (lambda (f) (format "./%s%d.v.X" f n)) (pre-not-compiled n)))

(defun all-unlocked (n)
  "All files that should be unlocked after compilation for part N."
  (mapcar (lambda (f) (format "./%s%d.v" f n)) pre-all-unlocked))


;;; Define the test

(defun test-failure-processing (n)
  "Test partially successful and partially failing compilation, part N."
  ;; (setq cct--debug-tests t)
  ;; (setq coq--debug-auto-compilation t)
  (find-file (format "a%d.v" n))
  (message "coqdep: %s\ncoqc: %s\nPATH %s\nexec-path: %s"
           coq-dependency-analyzer
           coq-compiler
           (getenv "PATH")
           exec-path)

  (message "\n%d. Partially failing compilation part %d\n" n n)
  (cct-process-to-line 28)
  (cct-check-locked 25 'locked)
  (cct-check-locked 26 'unlocked)
  (cct-locked-ancestors 25 (b-ancestors n))
  (cct-check-files-locked 24 'locked (b-ancestors n))
  (cct-check-files-locked 1 'unlocked (all-unlocked n))
  (cct-files-are-readable (all-compiled-vo n))
  (cct-files-dont-exist (not-compiled n)))


(ert-deftest cct-failure-processing-01 () (test-failure-processing 1))
(ert-deftest cct-failure-processing-02 () (test-failure-processing 2))
(ert-deftest cct-failure-processing-03 () (test-failure-processing 3))
(ert-deftest cct-failure-processing-04 () (test-failure-processing 4))
(ert-deftest cct-failure-processing-05 () (test-failure-processing 5))
(ert-deftest cct-failure-processing-06 () (test-failure-processing 6))
(ert-deftest cct-failure-processing-07 () (test-failure-processing 7))
(ert-deftest cct-failure-processing-08 () (test-failure-processing 8))
(ert-deftest cct-failure-processing-09 () (test-failure-processing 9))
(ert-deftest cct-failure-processing-10 () (test-failure-processing 10))

(provide 'runtest)

;;; runtest.el ends here
