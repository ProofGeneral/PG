;;; proof-compat.el --- Operating system and Emacs version compatibility  -*- lexical-binding:t -*-

;; This file is part of Proof General.

;; Portions © Copyright 1994-2012  David Aspinall and University of Edinburgh
;; Portions © Copyright 2003-2018  Free Software Foundation, Inc.
;; Portions © Copyright 2001-2017  Pierre Courtieu
;; Portions © Copyright 2010, 2016  Erik Martin-Dorel
;; Portions © Copyright 2011-2013, 2016-2017  Hendrik Tews
;; Portions © Copyright 2015-2017  Clément Pit-Claudel

;; Author:      David Aspinall <David.Aspinall@ed.ac.uk> and others

;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:
;;
;; This file collects together compatibility hacks for different
;; operating systems and Emacs versions.  This is to help keep
;; track of them.
;;
;; The development policy for Proof General (since v3.7) is for the
;; main codebase to be written for the latest stable version of
;; GNU Emacs, following GNU Emacs advice on obsolete function calls.
;;
;; Since Proof General 4.0, XEmacs is not supported at all.
;;

;;; Code:

(require 'cl-lib)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Modifications and adjustments
;;;

;; Remove a custom setting.  Needed to support dynamic reconfiguration.
;; (We'd prefer that repeated defcustom calls acted like repeated
;;  "defvar treated as defconst" in XEmacs)
(defun pg-custom-undeclare-variable (symbol)
  "Remove a custom setting SYMBOL.
Done by removing all properties mentioned by custom library.
The symbol itself is left defined, in case it has been changed
in the current Emacs session."
  (dolist (prop
	   '(default
	     standard-value
	     force-value
	     variable-comment
	     saved-variable-comment
	     variable-documentation
	     group-documentation
	     custom-set
	     custom-get
	     custom-options
	     custom-requests
	     custom-group
	     custom-prefix
	     custom-tag
	     custom-links
	     custom-version
	     saved-value
	     theme-value
	     theme-face))
    (cl-remprop symbol prop)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; A naughty hack to completion.el
;;
;; At the moment IMO completion too eagerly adds stuff to
;; its database: the completion-before-command function
;; makes every suffix be added as a completion!

(eval-after-load "completion"
  '(defun completion-before-command ()
     (if (and (symbolp this-command) (get this-command 'completion-function))
	 (funcall (get this-command 'completion-function)))))

(provide 'proof-compat)
;;; proof-compat.el ends here
