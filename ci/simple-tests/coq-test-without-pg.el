;;; coq-test-without-pg.el --- Test Coq mode without Proof General  -*- lexical-binding: t -*-
;;
;; This file is part of Proof General.
;;
;; License:     GPL (GNU GENERAL PUBLIC LICENSE)

;;; Commentary:
;;
;; Test the part of Coq mode common to all backends, with the Proof
;; General backend disabled (`coq-use-pg' nil): it must not load the
;; proof shell, and the abbrevs, the terminator command, the query
;; commands and their keys must be there.

;;; Code:

;; before loading coq-mode.el, which defines it
(defvar coq-use-pg)
(setq coq-use-pg nil)

(require 'proof-site)
(require 'ert)

(defvar coq-mode-abbrev-table)
(defvar coq-query-function)
(defvar coq-use-yasnippet)

(declare-function coq-run-query "coq-queries")
(declare-function coq-templates-setup "coq-templates")
(declare-function coq-terminator-insert "coq-mode")
(declare-function coq-About "coq-queries")
(declare-function coq-Check "coq-queries")
(declare-function coq-insert-intros "coq-queries")


;;; utility functions

(defmacro coq-test-without-pg-with-file (&rest body)
  "Run BODY in a buffer visiting a Coq file."
  (declare (indent 0) (debug t))
  `(let ((file (make-temp-file "without_pg" nil ".v" "Definition x := 0.\n"))
         buffer)
     (unwind-protect
         (progn
           (find-file file)
           (setq buffer (current-buffer))
           ,@body)
       (when buffer
         (with-current-buffer buffer
           (set-buffer-modified-p nil))
         (kill-buffer buffer))
       (delete-file file))))


;;; tests

(ert-deftest coq-test-without-pg-no-shell ()
  "Coq mode without the Proof General backend loads no proof shell."
  (coq-test-without-pg-with-file
    (should (eq major-mode 'coq-mode))
    (should-not (featurep 'proof))
    (should-not (featurep 'proof-shell))
    (should-not (featurep 'proof-script))))

(ert-deftest coq-test-without-pg-abbrevs ()
  "The abbrevs of Coq mode are there."
  (coq-test-without-pg-with-file
    (should (eq local-abbrev-table coq-mode-abbrev-table))
    (should (equal (abbrev-expansion "is" coq-mode-abbrev-table) "intros "))
    ;; no yasnippet in the tests, `coq-templates-setup' just says so
    (let ((coq-use-yasnippet nil))
      (coq-templates-setup))))

(ert-deftest coq-test-without-pg-commands ()
  "The common commands and their keys are there, without backend to
run the queries."
  (coq-test-without-pg-with-file
    (should (eq (key-binding ".") #'coq-terminator-insert))
    (should (eq (key-binding (kbd "C-c C-a C-b")) #'coq-About))
    (should (eq (key-binding (kbd "C-c C-a C-c")) #'coq-Check))
    (should (eq (key-binding (kbd "C-c C-a C-i")) #'coq-insert-intros))
    ;; no backend
    (should-not coq-query-function)
    (should-error (coq-run-query "Check x") :type 'user-error)
    ;; the terminator is just inserted
    (goto-char (point-max))
    (let ((last-command-event ?.))
      (call-interactively (key-binding ".")))
    (should (string-suffix-p "0.\n." (buffer-string)))))

(provide 'coq-test-without-pg)

;;; coq-test-without-pg.el ends here
