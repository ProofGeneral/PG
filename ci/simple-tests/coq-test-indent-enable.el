;;; coq-test-indent-enable.el --- Test of coq-indent-enable  -*- lexical-binding: t -*-
;;
;; This file is part of Proof General.
;;
;; License:     GPL (GNU GENERAL PUBLIC LICENSE)

;;; Commentary:
;;
;; With `coq-indent-enable' nil, the lines are indented like the
;; previous one, and not again while typing.  Without backend: the
;; indentation is the same in all of them.

;;; Code:

;; before loading coq-mode.el, which defines it
(defvar coq-use-pg)
(setq coq-use-pg nil)

(require 'proof-site)
(require 'ert)

(defvar coq-indent-enable)
(defvar electric-indent-inhibit)

(defconst indent-test-script
  "Lemma l : True.\nProof.\nintros.\n      exact I.\nQed.\n"
  "A script to indent.")

(defun indent-test (enable)
  "The text of `indent-test-script' indented, with `coq-indent-enable' ENABLE."
  (let ((coq-indent-enable enable))
    (with-temp-buffer
      (coq-mode)
      (insert indent-test-script)
      (indent-region (point-min) (point-max))
      (buffer-string))))

(ert-deftest coq-indent-enabled ()
  "By default, the lines are indented according to the syntax."
  (should (string-match-p "^  intros\\.\n  exact I\\.$" (indent-test t))))

(ert-deftest coq-indent-disabled ()
  "With `coq-indent-enable' nil, each line is indented like the previous
one, not according to the syntax, and not again while typing."
  (should (equal (indent-test nil)
                 "Lemma l : True.\nProof.\nintros.\nexact I.\nQed.\n"))
  (let ((coq-indent-enable nil))
    (with-temp-buffer
      (coq-mode)
      (should electric-indent-inhibit)
      (insert "Proof.\n    intros.")
      ;; a newline keeps the indentation, TAB does not change it again
      (newline nil t)
      (should (= (current-indentation) 4))
      (indent-for-tab-command)
      (should (= (current-indentation) 4))
      (insert "exact I.")
      (indent-for-tab-command)
      (should (equal (buffer-substring (line-beginning-position) (point))
                     "    exact I.")))))

;;; coq-test-indent-enable.el ends here
