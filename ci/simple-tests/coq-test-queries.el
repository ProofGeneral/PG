;;; coq-test-queries.el --- Test the query commands  -*- lexical-binding: t -*-
;;
;; This file is part of Proof General.
;;
;; License:     GPL (GNU GENERAL PUBLIC LICENSE)

;;; Commentary:
;;
;; Test that the query commands (About, Check, Search...) are bound in
;; `coq-mode-map', common to all backends, that Proof General sets
;; `coq-query-function', and that the printing flags of a query are
;; set for this query only.  Test also the option `coq-remap-mouse-1'
;; and the insertions built from the answer of a query.

;;; Code:

(defvar proof-three-window-enable)
(setq proof-three-window-enable nil)

(require 'proof-site)
(proof-ready-for-assistant 'coq)

(require 'ert)
(require 'cl-lib)
(require 'subr-x)

(defvar proof-second-action-list-active)
(defvar proof-action-list)
(defvar proof-response-buffer)
(defvar proof-goals-buffer)
(defvar coq-mode-map)
(defvar coq-query-function)
(defvar coq-query-result-function)
(defvar coq-show-at-point-function)
(defvar coq-ask-printing-width-function)
(defvar coq-shell-current-line-width)
(defvar coq-use-yasnippet)

(declare-function proof-shell-exit "proof-shell")
(declare-function proof-shell-invisible-command "proof-shell")
(declare-function coq-flag-is-on-p "coq")
(declare-function coq-pg-query "coq")
(declare-function coq-Show "coq-queries")
(declare-function coq-ask-printing-width "coq-goals")
(declare-function coq-insert-intros "coq")
(declare-function coq-insert-tactic "coq-templates")
(declare-function coq-insert-as-in-next-command "coq")
(declare-function coq-run-query "coq-queries")
(declare-function coq-About "coq-queries")
(declare-function coq-Check "coq-queries")
(declare-function coq-Search "coq-queries")
(declare-function coq-id-under-mouse-query "coq-queries")
(declare-function coq-query-result "coq-queries")
(declare-function coq-insert-match "coq-queries")
(declare-function coq-insert-named-goal-selectors "coq-queries")
(declare-function coq-pg-query-result "coq")
(declare-function coq-pg-show-at-point "coq")
(declare-function coq-ask-adapt-printing-width-and-show "coq")
(declare-function coq-query-flags-from-prefix "coq-queries")
(declare-function proof-electric-terminator-toggle "pg-user")
(declare-function proof-double-hit-toggle "pg-double-hit")


;;; utility functions

(defun coq-test-queries-wait ()
  "Wait until processing is complete."
  (while (or proof-second-action-list-active
             (consp proof-action-list))
    (accept-process-output nil 0.1)))

(defmacro coq-test-queries-with-file (&rest body)
  "Run BODY in a buffer visiting a Coq file, processed to its end."
  (declare (indent 0) (debug t))
  `(let ((file (make-temp-file "queries" nil ".v"
                               "Definition x := 0.\nLemma l : forall n : nat, n = x -> True.\n"))
         buffer)
     (unwind-protect
         (progn
           (find-file file)
           (setq buffer (current-buffer))
           (goto-char (point-max))
           (proof-goto-point)
           (coq-test-queries-wait)
           ,@body)
       (when buffer
         (with-current-buffer buffer
           (set-buffer-modified-p nil))
         (kill-buffer buffer))
       (when (proof-shell-live-buffer)
         (proof-shell-exit t))
       (delete-file file))))

(defun coq-test-queries-response ()
  "The text of the response buffer."
  (with-current-buffer proof-response-buffer
    (buffer-substring-no-properties (point-min) (point-max))))


;;; tests

(ert-deftest coq-test-queries-common-keys ()
  "The query keys are in `coq-mode-map', the other C-c C-a keys of
Proof General still work."
  (coq-test-queries-with-file
    (should (eq (lookup-key coq-mode-map (kbd "C-c C-a C-b")) #'coq-About))
    (should (eq (key-binding (kbd "C-c C-a C-b")) #'coq-About))
    (should (eq (key-binding (kbd "C-c C-a C-c")) #'coq-Check))
    (should (eq (key-binding (kbd "C-c C-a C-a")) #'coq-Search))
    (should (eq (key-binding (kbd "C-c C-a C-i")) #'coq-insert-intros))
    (should (eq (key-binding (kbd "C-c C-a C-m")) #'coq-insert-match))
    (should (eq (key-binding (kbd "C-c C-a C-g"))
                #'coq-insert-named-goal-selectors))
    (should (eq (key-binding (kbd "C-c C-a C-t")) #'coq-insert-tactic))
    (should (eq (lookup-key coq-mode-map (kbd "C-c C-a C-s")) #'coq-Show))
    (should (eq (key-binding (kbd "C-c C-a C-s")) #'coq-Show))
    (should (eq (key-binding (kbd "C-c C-a C-w")) #'coq-ask-printing-width))
    ;; Proof General only, in `coq-keymap'
    (should (eq (key-binding (kbd "C-c C-a [")) #'coq-insert-as-in-next-command))))

(ert-deftest coq-test-queries-function-set ()
  "Proof General sets `coq-query-function' in its buffers."
  (coq-test-queries-with-file
    (dolist (buffer (list (current-buffer) proof-goals-buffer
                          proof-response-buffer))
      (with-current-buffer buffer
        (should (eq coq-query-function #'coq-pg-query))
        (should (eq coq-query-result-function #'coq-pg-query-result))
        (should (eq coq-show-at-point-function #'coq-pg-show-at-point))
        (should (eq coq-ask-printing-width-function
                    #'coq-ask-adapt-printing-width-and-show))))
    (with-temp-buffer
      (should-error (coq-run-query "Check x") :type 'user-error)
      (should-error (coq-query-result "Check x") :type 'user-error))))

(ert-deftest coq-test-queries-show ()
  "`coq-Show' shows the answer to the command at point in the locked
region, asks for a goal number elsewhere."
  (coq-test-queries-with-file
    ;; after the locked region
    (goto-char (point-max))
    (insert "\n")
    (should-not (coq-pg-show-at-point))
    (cl-letf (((symbol-function 'read-string) (lambda (&rest _) "1")))
      (coq-Show nil))
    (coq-test-queries-wait)
    (should (string-match-p "n = x -> True" (coq-test-queries-response)))
    ;; on the lemma, in the locked region
    (with-current-buffer proof-response-buffer
      (let ((inhibit-read-only t)) (erase-buffer)))
    (goto-char (point-min))
    (search-forward "Lemma")
    (cl-letf (((symbol-function 'read-string)
               (lambda (&rest _) (error "No goal number asked here"))))
      (coq-Show nil))))

(ert-deftest coq-test-queries-printing-width ()
  "`coq-ask-printing-width' sets the printing width with Proof General."
  (coq-test-queries-with-file
    (cl-letf (((symbol-function 'read-number) (lambda (&rest _) 51)))
      (coq-ask-printing-width))
    (coq-test-queries-wait)
    (should (eql coq-shell-current-line-width 51))
    ;; one column less, see `coq-adapt-printing-width'
    (should (string-match-p "\\_<50\\_>"
                            (coq-query-result "Test Printing Width")))))

(ert-deftest coq-test-queries-result ()
  "`coq-query-result' returns the answer of a query, nil on an error."
  (coq-test-queries-with-file
    (should (string-match-p "x\\s-*:\\s-*nat" (coq-query-result "Check x")))
    (should-not (coq-query-result "Check unknown_identifier"))))

(ert-deftest coq-test-queries-insert-intros ()
  "`coq-insert-intros' inserts the names given by Show Intros."
  (coq-test-queries-with-file
    (proof-electric-terminator-toggle 0)
    (proof-double-hit-toggle 0)
    (coq-insert-intros)
    (should (string-match-p "intros n H\\.\\'"
                            (string-trim-right (buffer-string))))))

(ert-deftest coq-test-queries-insert-match ()
  "`coq-insert-match' inserts the match given by Show Match."
  (coq-test-queries-with-file
    (let ((coq-use-yasnippet nil))
      (cl-letf (((symbol-function 'read-string) (lambda (&rest _) "nat")))
        (coq-insert-match))
      (should (string-match-p "match\\s-+with" (buffer-string)))
      (should (string-match-p "| O =>" (buffer-string)))
      (should (string-match-p "| S [[:alnum:]_]+ =>" (buffer-string))))))

(ert-deftest coq-test-queries-run ()
  "A query displays its answer."
  (coq-test-queries-with-file
    (coq-run-query "Check x" nil #'ignore)
    (coq-test-queries-wait)
    (should (string-match-p "x\\s-*:\\s-*nat" (coq-test-queries-response)))))

(ert-deftest coq-test-queries-flags ()
  "The printing flags of a query are for this query only, a flag already
set stays set."
  (coq-test-queries-with-file
    (coq-run-query "Check (1 + 1)" '("Set Printing All") #'ignore)
    (coq-test-queries-wait)
    (should (string-match-p "Nat\\.add" (coq-test-queries-response)))
    (should-not (coq-flag-is-on-p "Test Printing All"))
    ;; already set: not unset after the query
    (proof-shell-invisible-command "Set Printing All." 'wait)
    (coq-run-query "Check (1 + 1)" '("Set Printing All") #'ignore)
    (coq-test-queries-wait)
    (should (coq-flag-is-on-p "Test Printing All"))))

(ert-deftest coq-test-queries-flags-from-prefix ()
  "The prefix argument gives the printing flags of a query."
  (should-not (coq-query-flags-from-prefix nil))
  (should (equal (coq-query-flags-from-prefix '(4)) '("Set Printing All")))
  ;; not only interactively
  (should (equal (coq-query-flags-from-prefix t) '("Set Printing All")))
  (cl-letf (((symbol-function 'completing-read-multiple)
             (lambda (&rest _) '("Implicit" "No Notations"))))
    (should (equal (coq-query-flags-from-prefix '(16))
                   '("Set Printing Implicit" "Unset Printing Notations")))))

(ert-deftest coq-test-queries-unset-flag ()
  "A flag unsetting an option, like No Notations, is undone after the
query."
  (coq-test-queries-with-file
    (coq-run-query "Check (1 + 1)" '("Unset Printing Notations") #'ignore)
    (coq-test-queries-wait)
    (should (string-match-p "Nat\\.add" (coq-test-queries-response)))
    (should (coq-flag-is-on-p "Test Printing Notations"))))

(ert-deftest coq-test-queries-mouse ()
  "`coq-remap-mouse-1' binds the queries at mouse click when set, in the
script, goals and response buffers, and unbinds them when unset."
  (coq-test-queries-with-file
    (let ((key [(shift down-mouse-1)]))
      (unwind-protect
          (progn
            (customize-set-variable 'coq-remap-mouse-1 t)
            (should (eq (lookup-key coq-mode-map key) #'coq-id-under-mouse-query))
            (should (eq (key-binding key) #'coq-id-under-mouse-query))
            (with-current-buffer proof-goals-buffer
              (should (eq (key-binding key) #'coq-id-under-mouse-query)))
            (with-current-buffer proof-response-buffer
              (should (eq (key-binding key) #'coq-id-under-mouse-query)))
            (customize-set-variable 'coq-remap-mouse-1 nil)
            (should-not (lookup-key coq-mode-map key))
            (should-not (eq (key-binding key) #'coq-id-under-mouse-query)))
        (customize-set-variable 'coq-remap-mouse-1 nil)))))

(provide 'coq-test-queries)

;;; coq-test-queries.el ends here
