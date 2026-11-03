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
;; set for this query only, and the option `coq-remap-mouse-1'.

;;; Code:

(defvar proof-three-window-enable)
(setq proof-three-window-enable nil)

(require 'proof-site)
(proof-ready-for-assistant 'coq)

(require 'ert)

(defvar proof-second-action-list-active)
(defvar proof-action-list)
(defvar proof-response-buffer)
(defvar proof-goals-buffer)
(defvar coq-mode-map)
(defvar coq-query-function)

(declare-function proof-shell-exit "proof-shell")
(declare-function proof-shell-invisible-command "proof-shell")
(declare-function coq-flag-is-on-p "coq")
(declare-function coq-pg-query "coq")
(declare-function coq-Show "coq")
(declare-function coq-insert-intros "coq")
(declare-function coq-run-query "coq-queries")
(declare-function coq-About "coq-queries")
(declare-function coq-Check "coq-queries")
(declare-function coq-Search "coq-queries")
(declare-function coq-id-under-mouse-query "coq-queries")


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
                               "Definition x := 0.\nLemma l : x = 0.\n"))
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
    ;; Proof General only, in `coq-keymap'
    (should (eq (key-binding (kbd "C-c C-a C-i")) #'coq-insert-intros))
    (should (eq (key-binding (kbd "C-c C-a C-s")) #'coq-Show))))

(ert-deftest coq-test-queries-function-set ()
  "Proof General sets `coq-query-function' in its buffers."
  (coq-test-queries-with-file
    (should (eq coq-query-function #'coq-pg-query))
    (with-current-buffer proof-goals-buffer
      (should (eq coq-query-function #'coq-pg-query)))
    (with-current-buffer proof-response-buffer
      (should (eq coq-query-function #'coq-pg-query)))
    (with-temp-buffer
      (should-error (coq-run-query "Check x") :type 'user-error))))

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
