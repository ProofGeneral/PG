;; This file is part of Proof General.

;; Portions © Copyright 1994-2012, David Aspinall and University of Edinburgh
;; Portions © Copyright 1985-2014, Free Software Foundation, Inc
;; Portions © Copyright 2001-2006, Pierre Courtieu
;; Portions © Copyright 2010, Erik Martin-Dorel
;; Portions © Copyright 2012, Hendrik Tews
;; Portions © Copyright 2017, Clément Pit-Claudel
;; Portions © Copyright 2016-2017, Massachusetts Institute of Technology

;; Proof General is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, version 2.

;; Proof General is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with Proof General. If not, see <http://www.gnu.org/licenses/>.
;;; pg-dev.el --- Developer settings for Proof General
;;
;; Copyright (C) 2008-2011 LFCS Edinburgh.
;; Author:      David Aspinall <David.Aspinall@ed.ac.uk> and others
;; License:     GPL (GNU GENERAL PUBLIC LICENSE)
;;
;; $Id$
 ;;
;;; Commentary:
;;
;; Some configuration of Emacs Lisp mode for developing PG, not needed
;; for ordinary users.
;;

;;; Code:

(require 'whitespace)

(eval-when-compile
  (require 'cl-lib))

(eval-when-compile
  (require 'proof-site))

(with-no-warnings
  (setq proof-general-debug t))

;; Use checkdoc, eldoc, Flyspell, whitespace, copyright update
;; and byte compilation on save:

(add-hook 'emacs-lisp-mode-hook
	  (lambda ()
            (checkdoc-minor-mode 1)
            (eldoc-mode 1)
            (flyspell-prog-mode)
            (customize-set-variable 'whitespace-action '(cleanup))
            (define-key emacs-lisp-mode-map [(control c)(control c)]
              'emacs-lisp-byte-compile)
            (add-hook 'write-file-functions
                      'whitespace-write-file-hook nil t)
            (add-hook 'before-save-hook
                      'copyright-update nil t)))

;; Fill in template for new files

(add-hook 'find-file-hook 'auto-insert)

;; Configure indentation for our macros

(put 'proof-if-setting-configured 'lisp-indent-function 1)
(put 'proof-eval-when-ready-for-assistant 'lisp-indent-function 1)
(put 'proof-define-assistant-command 'lisp-indent-function 'defun)
(put 'proof-define-assistant-command-witharg 'lisp-indent-function 'defun)
(put 'defpgcustom 'lisp-indent-function 'defun)
(put 'proof-map-buffers 'lisp-indent-function 'defun)
(put 'proof-with-current-buffer-if-exists 'lisp-indent-function 'defun)

(defconst pg-dev-lisp-font-lock-keywords
  (list
    (concat "(\\(def" ;; also proof-def
	   ;; Function like things:
	   ;; Variable like things
	   "\\(pgcustom\\|pacustom\\)\\)"
    ;; Any whitespace and declared object.
    "[ \t'\(]*"
    "\\(\\sw+\\)?")
   '(1 font-lock-keyword-face)
   '(3 (cond ((match-beginning 2) 'font-lock-variable-name-face)
	     (t 'font-lock-function-name-face))
       nil t)))

;; Not working, see font-lock.el for usual emacs lisp settings.
;;(add-hook 'emacs-lisp-mode-hook
;;	  (lambda ()
;;	    (font-lock-add-keywords nil
;;				    'pg-dev-lisp-font-lock-keywords)))


;;
;; Path set for a clean environment to byte-compile within Emacs
;; without loading.
;;

(defun pg-loadpath ()
  (interactive)
  (proof-add-to-load-path "../generic/")
  (proof-add-to-load-path "../lib/"))


;;;
;;; Unload utility (not wholly successful)
;;;

;; TODO update the list of modules
(defun unload-pg ()
  "Attempt to unload Proof General (for development use only)."
  (interactive)
  (mapcar
   (lambda (feat) (condition-case nil
		    (unload-feature feat 'force)
		    (error nil)))
   '(proof-splash pg-assoc pg-xml proof-depends proof-indent proof-site
     proof-shell proof-menu pg-pbrpm proof-script
     proof-autoloads pg-response pg-goals proof-toolbar
     proof-easy-config proof-config proof
     proof-utils proof-syntax pg-user pg-custom
     proof-maths-menu proof-unicode-tokens
     pg-thymodes pg-autotest
     ;;
     coq-abbrev coq-db coq-unicode-tokens coq-local-vars coq coq-syntax
     coq-indent coq-autotest)))



;;
;; Proling interesting packages
;;

(require 'elp)

;;;###autoload
(defun profile-pg ()
  "Configure Proof General for profiling.  Use M-x elp-results to see results."
  (interactive)
  (elp-instrument-package "proof-")
  (elp-instrument-package "pg-")
  (elp-instrument-package "scomint")
  (elp-instrument-package "unicode-tokens")
  (elp-instrument-package "coq")
  (elp-instrument-package "isar")
  (elp-instrument-package "span")
  (elp-instrument-package "replace-") ; for replace-regexp etc
  (elp-instrument-package "re-search-") ; for re-search-forwad etc
  (elp-instrument-package "skip-chars-") ; for skip chars etc
  (elp-instrument-list 
   '(string-match match-string re-search-forward re-search-backward
     skip-chars-forward skip-chars-backward
     goto-char insert 
     set-marker marker-position
     nreverse nconc mapc
     member
     redisplay
     sit-for
     overlay-put overlay-start overlay-end make-overlay
     buffer-live-p kill-buffer
     process-status get-buffer-process 
     delete-overlay move-overlay
     accept-process-output))
  (elp-instrument-package "font-lock"))

;; improve readability of profile results, give milliseconds
(defun elp-pack-number (number width)
  (format (concat "%" (number-to-string (- width 3)) ".2f")
	  (* 100 (string-to-number number))))


;;
;; Make references to bugs clickable; [e.g., trac #1]
;;

(defun pg-bug-references ()
  (interactive)
  (if (fboundp 'bug-reference-mode)
      (with-no-warnings
	(bug-reference-mode 1)
	(setq bug-reference-bug-regexp
	      "\\(?:[Tt]rac ?#\\)\\([0-9]+\\)"
	      bug-reference-url-format
	      "http://proofgeneral.inf.ed.ac.uk/trac/ticket/%s"))))

(add-hook 'emacs-lisp-mode-hook 'pg-bug-references)
(add-hook 'isar-mode-hook 'pg-bug-references)
(add-hook 'coq-mode-hook 'pg-bug-references)

(add-hook 'emacs-lisp-mode-hook 'goto-address-mode)


(provide 'pg-dev)

;;; pg-dev.el ends here
