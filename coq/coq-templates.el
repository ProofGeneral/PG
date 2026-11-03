;;; coq-templates.el --- Abbrevs, templates and insertions for Coq  -*- lexical-binding:t -*-

;; This file is part of Proof General.

;; Portions © Copyright 1994-2012  David Aspinall and University of Edinburgh
;; Portions © Copyright 2003-2021  Free Software Foundation, Inc.
;; Portions © Copyright 2001-2026  Pierre Courtieu

;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:
;;
;; The abbrevs, the yasnippet templates and the insertion commands of
;; Coq mode, mostly built from the keyword databases of coq-syntax.el.
;; They are common to all Coq backends: `coq-mode' calls
;; `coq-templates-setup'.

;;; Code:

(require 'cl-lib)
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

;;; Insertion commands
;;
;; They insert text from the keyword databases of coq-syntax.el, or
;; after asking questions, without any backend.

(defvar coq-load-path)                  ; in coq-system.el

(define-obsolete-variable-alias 'module-kinds-table
  'coq-module-kinds-table "4.6")
(define-obsolete-variable-alias 'modtype-kinds-table
  'coq-modtype-kinds-table "4.6")
(define-obsolete-variable-alias 'reqkinds-kinds-table
  'coq-require-kinds-table "4.6")

(defconst coq-module-kinds-table
  '(("Section" 0) ("Module" 1) ("Module Type" 2) ("Declare Module" 3))
  "Enumerates the different kinds of modules.")

(defconst coq-modtype-kinds-table
  '(("" 1) (":" 2) ("<:" 3))
  "Enumerates the different kinds of type information for modules.")

(defun coq-postfix-.v-p (s)
  (string-match-p "\\.v\\'" s))

(defun coq-directories-files (l)
  (let* ((file-list-list (mapcar #'directory-files l))
         (file-list (apply #'append file-list-list))
         (filtered-list (cl-remove-if-not #'coq-postfix-.v-p file-list)))
  filtered-list))

(defun coq-remove-dot-v-extension (s)
  (substring s 0 -2))

(defun coq-load-path-to-paths (ldpth)
  (if (listp ldpth) (car ldpth) ldpth))

(defun coq-build-accessible-modules-list ()
  (let* ((pth (or coq-load-path '(".")))
         (cleanpth (mapcar #'coq-load-path-to-paths pth))
         (existingpth (cl-remove-if-not #'file-exists-p cleanpth))
         (file-list (coq-directories-files existingpth)))
    (mapcar #'coq-remove-dot-v-extension file-list)))

(defun coq-insert-section-or-module ()
  "Insert a module or a section after asking right questions."
  (interactive)
  (let*
      ((mods (completing-read "Kind of module (TAB to see list): "
                              coq-module-kinds-table))
       (s (read-string  "Name: "))
       (typkind (if (string-equal mods "Section")
                    "" ;; if not a section
                  (completing-read "Kind of type (optional, TAB to see list): "
                                   coq-modtype-kinds-table))))
    (if (string-equal typkind "")
        (progn
          (insert mods " " s ".\n")
          (let ((pt (point)))
            (insert "\nEnd " s ".")
            (goto-char pt)))
      (insert mods " " s " " typkind ".\n")
      (let ((pt (point)))
        (insert "\nEnd " s ".")
        (goto-char pt)))))

(defconst coq-require-kinds-table
  '(("Require Import") ("Require Export") ("Require") ("Import"))
  "Enumerates the different kinds of requiring a module.")

(defun coq-insert-requires ()
  "Insert requires to modules, iteratively."
  (interactive)
  (let* ((s)
         (reqkind
          (completing-read
           "Command (TAB to see list, default Require Import) : "
           coq-require-kinds-table nil nil nil nil "Require Import")))
    (cl-loop do
             (setq s (completing-read "Name (empty to stop) : "
                                      (coq-build-accessible-modules-list)))
             (unless (zerop (length s)) (insert (format "%s %s.\n" reqkind s)))
             while (not (string-equal s "")))))

;; TODO add module closing
(defun coq-end-Section ()
  "Ends a Coq section."
  (interactive)
  (let ((count 1)) ; The number of section already "Ended" + 1
    (let ((section
	   (save-excursion
	     (progn
	       (while (and (> count 0)
			   (search-backward-regexp
			    "Chapter\\|Section\\|End" 0 t))
		 (if (char-equal (char-after (point)) ?E)
		     (setq count (1+ count))
		   (setq count (1- count))))
	       (buffer-substring-no-properties
          (progn (beginning-of-line) (forward-word 1) (point))
          (progn (end-of-line) (point)))))))
      (insert (concat "End" section)))))


(defun coq-insert-solve-tactic ()
  "Ask for a closing tactic name, with completion, and insert at point.
Completion is on a quasi-exhaustive list of Coq closing tactics."
  (interactive)
  (coq-insert-from-db coq-solve-tactics-db "Closing tactic"))

(defun coq-insert-tactic ()
  "Insert a tactic name at point, with completion.
Questions may be asked to the user to select the tactic."
  (interactive)
  (coq-insert-from-db coq-tactics-db "Tactic"))

(defun coq-insert-tactical ()
  "Ask for a closing tactic name, with completion, and insert at point.
Completion is on a quasi-exhaustive list of Coq tacticals."
  (interactive)
  (coq-insert-from-db coq-tacticals-db "Tactical"))

(defun coq-insert-command ()
  "Ask for a command name, with completion, and insert it at point."
  (interactive)
  (coq-insert-from-db coq-commands-db "Command"))

(defun coq-insert-term ()
  "Ask for a term kind, with completion, and insert it at point."
  (interactive)
  (coq-insert-from-db coq-terms-db "Kind of term"))

(provide 'coq-templates)

;;; coq-templates.el ends here
