;; This file is part of Proof General.  -*- lexical-binding: t; -*-
;; 
;; © Copyright 2024  Hendrik Tews
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
;; Test that parallel background compilation reliably detects coqdep
;; errors. There are three tests that check the following coqdep errors:
;; - coqdep on a require fails because of a missing library (using a.v)
;; - coqdep on a dependency fails because of a syntax error (using b.v)
;; - coqdep on a dependency fails because of a missing library (using d.v)
;;
;; The following graph shows the file dependencies in these test:
;; 
;;   a   b   d
;;       |   |
;;       c   e


;; require cct-lib for the elisp compilation, otherwise this is present already
(require 'cct-lib "ci/compile-tests/cct-lib")

;;; set configuration
(cct-configure-proof-general)

;;; Define the tests

(ert-deftest cct-coqdep-fail-on-require ()
  "coqdep error on missing library in a require command is detected."
  ;; (setq cct--debug-tests t)
  ;; (setq coq--debug-auto-compilation t)
  (let (coqdep-errror-in-response
        missing-module-in-response
        missing-module-in-coq)
    (find-file "a.v")
    (message "coqdep error detection test: non-existing module in require")
    (cct-process-to-line 21)
    (cct-check-locked 20 'unlocked)
    (with-current-buffer coq--compile-response-buffer
      ;; (message "|%s|" (buffer-string))
      (goto-char (1+ (length coq--compile-response-initial-content)))
      (setq coqdep-errror-in-response (looking-at "^coqdep "))
      (setq missing-module-in-response (search-forward "X25XX" nil t)))
    (with-current-buffer proof-shell-buffer
      (goto-char (point-min))
      (setq missing-module-in-coq (search-forward "X25XX" nil t)))
    (message
     (concat "CHECK RESULT: *coq-compile-response* %s report a coqdep problem\n"
     "\tand the missing module X25XX %s in *coq-compile-response*\n"
     "\tand it %s in *coq*")
     (if coqdep-errror-in-response "DOES" "DOES NOT")
     (if missing-module-in-response "IS" "IS NOT")
     (if missing-module-in-coq "IS" "IS NOT"))
    (should (and coqdep-errror-in-response
                 missing-module-in-response
                 (not missing-module-in-coq)))))


(ert-deftest cct-coqdep-syntax-error-dependency ()
  "coqdep syntax error on a dependency is detected."
  ;; (setq cct--debug-tests t)
  ;; (setq coq--debug-auto-compilation t)
  (let (coqdep-errror-in-response
        syntax-error-in-response
        dependency-in-coq)
    (find-file "b.v")
    (message "coqdep error detection test: non-existing module in dependency")
    (cct-process-to-line 21)
    (cct-check-locked 20 'unlocked)
    (with-current-buffer coq--compile-response-buffer
      ;; (message "|%s|" (buffer-string))
      (goto-char (1+ (length coq--compile-response-initial-content)))
      (setq coqdep-errror-in-response (looking-at "^coqdep "))
      (setq syntax-error-in-response (search-forward "Syntax error" nil t)))
    (with-current-buffer proof-shell-buffer
      (goto-char (point-min))
      (setq dependency-in-coq (search-forward "Require c." nil t)))
    (message
     (concat "CHECK RESULT: *coq-compile-response* %s report a coqdep problem\n"
      "\tand *coq-compile-response* %s contain a syntax error\n"
      "\tand 'Require c' %s in *coq*")
     (if coqdep-errror-in-response "DOES" "DOES NOT")
     (if syntax-error-in-response "DOES" "DOES NOT")
     (if dependency-in-coq "IS" "IS NOT"))
    (should (and coqdep-errror-in-response
                 syntax-error-in-response
                 (not dependency-in-coq)))))


(ert-deftest cct-coqdep-fail-on-require-in-dependency ()
  "coqdep error because of a missing library in a dependency is detected."
  (let (coqdep-errror-in-response
        missing-module-in-response
        dependency-in-coq)
    (find-file "d.v")
    (message "coqdep error detection test: non-existing module in dependency")
    (cct-process-to-line 21)
    (cct-check-locked 20 'unlocked)
    (with-current-buffer coq--compile-response-buffer
      ;; (message "|%s|" (buffer-string))
      (goto-char (1+ (length coq--compile-response-initial-content)))
      (setq coqdep-errror-in-response (looking-at "^coqdep "))
      (setq missing-module-in-response (search-forward "X25XX" nil t)))
    (with-current-buffer proof-shell-buffer
      (goto-char (point-min))
      (setq dependency-in-coq (search-forward "Require d." nil t)))
    (message
     (concat "CHECK RESULT: *coq-compile-response* %s report a coqdep problem\n"
             "\tand missing module X25XX %s in *coq-compile-response*\n"
             "\tand require %s in *coq*")
     (if coqdep-errror-in-response "DOES" "DOES NOT")
     (if missing-module-in-response "IS" "IS NOT")
     (if dependency-in-coq "IS" "IS NOT"))
    (should (and coqdep-errror-in-response
                 missing-module-in-response
                 (not dependency-in-coq)))))
