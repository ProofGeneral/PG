;;; coq-templates.el --- Abbrevs and yasnippet templates for Coq  -*- lexical-binding:t -*-

;; This file is part of Proof General.

;; Portions © Copyright 1994-2012  David Aspinall and University of Edinburgh
;; Portions © Copyright 2003-2021  Free Software Foundation, Inc.
;; Portions © Copyright 2001-2026  Pierre Courtieu

;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:
;;
;; The abbrevs and the yasnippet templates of Coq mode, built from the
;; keyword databases of coq-syntax.el.  They are common to all Coq
;; backends: `coq-mode' calls `coq-templates-setup'.

;;; Code:

(require 'coq-syntax)

(defcustom coq-yasnippet-use-default-templates t
  "Should ProofGeneral coq mode use its yasnippets default templates.

Set this to nil if you don't want the default yasnippets templates."
  :type 'boolean
  :group 'coq)

(defconst coq-tactics-abbrev-table
  (coq-build-abbrev-table-from-db (append coq-tactics-db coq-solve-tactics-db)))

(defconst coq-tacticals-abbrev-table
  (coq-build-abbrev-table-from-db coq-tacticals-db))

(defconst coq-commands-abbrev-table
  (coq-build-abbrev-table-from-db coq-commands-db))

(defconst coq-terms-abbrev-table
  (coq-build-abbrev-table-from-db coq-terms-db))

(defun coq-define-yasnippets-from-db ()
  "Register yas snippets from default ProofGeneral coq db.

yasnippet is a framework for inserting code templates for Emacs.  This
will add hundreds of yas snippets, generated from ProofGeneral list of
coq commands (which is not necessarily always up to date).  You probably
want to use your own set of snippets at some point but it is a good
start."
  (when (fboundp 'yas-define-snippets)
    (yas-define-snippets 'coq-mode (coq-yas-snippet-table-from-db coq-tactics-db))
    (yas-define-snippets 'coq-mode (coq-yas-snippet-table-from-db coq-solve-tactics-db))
    (yas-define-snippets 'coq-mode (coq-yas-snippet-table-from-db coq-solve-cheat-tactics-db))
    (yas-define-snippets 'coq-mode (coq-yas-snippet-table-from-db coq-tacticals-db))
    (yas-define-snippets 'coq-mode (coq-yas-snippet-table-from-db coq-decl-db))
    (yas-define-snippets 'coq-mode (coq-yas-snippet-table-from-db coq-defn-db))
    (yas-define-snippets 'coq-mode (coq-yas-snippet-table-from-db coq-goal-starters-db))
    (yas-define-snippets 'coq-mode (coq-yas-snippet-table-from-db coq-queries-commands-db))
    (yas-define-snippets 'coq-mode (coq-yas-snippet-table-from-db coq-other-commands-db))
    (yas-define-snippets 'coq-mode (coq-yas-snippet-table-from-db coq-ssreflect-commands-db))
    (yas-define-snippets 'coq-mode (coq-yas-snippet-table-from-db coq-terms-db))
    (yas-define-snippets 'coq-mode (coq-yas-snippet-table-from-db coq-user-tactics-db))
    (yas-define-snippets 'coq-mode (coq-yas-snippet-table-from-db coq-user-commands-db))
    (yas-define-snippets 'coq-mode (coq-yas-snippet-table-from-db coq-user-tacticals-db))
    (yas-define-snippets 'coq-mode (coq-yas-snippet-table-from-db coq-user-solve-tactics-db))
    (yas-define-snippets 'coq-mode (coq-yas-snippet-table-from-db coq-user-cheat-tactics-db))))

;;; The abbrev table built from keywords tables
(define-abbrev-table 'coq-mode-abbrev-table
  (append coq-tactics-abbrev-table coq-tacticals-abbrev-table
          coq-commands-abbrev-table coq-terms-abbrev-table)
  "Abbrev table for Coq.")

(defvar coq-use-yasnippet)

(defun coq-templates-setup ()
  "Register the default yasnippet templates, if wanted.
See `coq-use-yasnippet' and `coq-yasnippet-use-default-templates'.
Otherwise the abbrevs of `coq-mode-abbrev-table' are available."
  (cond
   ((and coq-use-yasnippet (not (fboundp 'yas-define-snippets)))
    (message "Warning: `coq-use-yasnippet` is t but yasnippet not installed."))
   ((and coq-use-yasnippet (not coq-yasnippet-use-default-templates))
    (message "Default coq yasnippets NOT loaded as asked."))
   (coq-use-yasnippet
    (message "Loading default coq yasnippets.")
    (coq-define-yasnippets-from-db))
   (t (message "yasnippet not found by proofgeneral, falling back to simple abbrevs"))))

(provide 'coq-templates)

;;; coq-templates.el ends here
