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
;; and insertion commands and their keys must be there.

;;; Code:

;; before loading coq-mode.el, which defines it
(defvar coq-use-pg)
(setq coq-use-pg nil)

(require 'proof-site)
(require 'ert)
(require 'cl-lib)

(defvar coq-mode-abbrev-table)
(defvar coq-load-path)
(defvar coq-prog-args)
(defvar coq-query-function)
(defvar coq-use-yasnippet)

(declare-function coq-run-query "coq-queries")
(declare-function coq-templates-setup "coq-templates")
(declare-function coq-terminator-insert "coq-mode")
(declare-function coq-About "coq-queries")
(declare-function coq-Check "coq-queries")
(declare-function coq-Show "coq-queries")
(declare-function coq-ask-printing-width "coq-goals")
(declare-function coq-insert-intros "coq-queries")
(declare-function coq-insert-tactic "coq-templates")
(declare-function coq-insert-tactical "coq-templates")
(declare-function coq-insert-solve-tactic "coq-templates")
(declare-function coq-insert-term "coq-templates")
(declare-function coq-insert-command "coq-templates")
(declare-function coq-insert-requires "coq-templates")
(declare-function coq-insert-section-or-module "coq-templates")
(declare-function coq-end-Section "coq-templates")


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

(ert-deftest coq-test-without-pg-project-file ()
  "The project file sets the load path and the arguments."
  (let* ((dir (file-name-as-directory (make-temp-file "without_pg" t)))
         (file (expand-file-name "a.v" dir))
         buffer)
    (unwind-protect
        (progn
          (with-temp-file (expand-file-name "_RocqProject" dir)
            (insert "-R theories Foo\n-Q lib \"\"\n-arg -w -arg all\n"))
          (with-temp-file file (insert "Definition x := 0.\n"))
          (find-file file)
          (setq buffer (current-buffer))
          (should (equal coq-load-path
                         `((rec ,(expand-file-name "theories" dir) "Foo")
                           (recnoimport ,(expand-file-name "lib" dir) ""))))
          (should (equal coq-prog-args '("-w" "all")))
          (should-not (featurep 'proof)))
      (when buffer (kill-buffer buffer))
      (delete-directory dir t))))

(ert-deftest coq-test-without-pg-commands ()
  "The common commands and their keys are there, without backend to
run the queries."
  (coq-test-without-pg-with-file
    (should (eq (key-binding ".") #'coq-terminator-insert))
    (should (eq (key-binding (kbd "C-c C-a C-b")) #'coq-About))
    (should (eq (key-binding (kbd "C-c C-a C-c")) #'coq-Check))
    (should (eq (key-binding (kbd "C-c C-a C-s")) #'coq-Show))
    (should (eq (key-binding (kbd "C-c C-a C-w")) #'coq-ask-printing-width))
    (should (eq (key-binding (kbd "C-c C-a C-i")) #'coq-insert-intros))
    (should (eq (key-binding (kbd "C-c C-a C-(")) #'coq-insert-section-or-module))
    (should (eq (key-binding (kbd "C-c C-a C-)")) #'coq-end-Section))
    (should (eq (key-binding (kbd "C-c C-a C-t")) #'coq-insert-tactic))
    (should (eq (key-binding (kbd "C-c C-a t")) #'coq-insert-tactical))
    (should (eq (key-binding (kbd "C-c C-a !")) #'coq-insert-solve-tactic))
    (should (eq (key-binding (kbd "C-c C-a C-SPC")) #'coq-insert-term))
    (should (eq (key-binding (kbd "C-c C-a C-<return>")) #'coq-insert-command))
    (should (eq (key-binding (kbd "C-c C-a C-r")) #'coq-insert-requires))
    ;; no backend
    (should-not coq-query-function)
    (should-error (coq-run-query "Check x") :type 'user-error)
    (should-error (coq-ask-printing-width) :type 'user-error)
    ;; the terminator is just inserted
    (goto-char (point-max))
    (let ((last-command-event ?.))
      (call-interactively (key-binding ".")))
    (should (string-suffix-p "0.\n." (buffer-string)))))

(ert-deftest coq-test-without-pg-insertions ()
  "The insertion commands without query work without backend, and
indent."
  (coq-test-without-pg-with-file
    (goto-char (point-max))
    (insert "Lemma l : True.\nProof.\n")
    (cl-letf (((symbol-function 'completing-read)
               (lambda (&rest _) "reflexivity")))
      (coq-insert-tactic))
    ;; inserted and indented
    (should (string-suffix-p "Proof.\n  reflexivity" (buffer-string)))
    (insert "\nSection S.\n")
    (coq-end-Section)
    (should (string-suffix-p "Section S.\nEnd S." (buffer-string)))))

(provide 'coq-test-without-pg)

;;; coq-test-without-pg.el ends here
