;; doc/docstring-magic.el  -- hack for using texi-docstring-magic.
;;
;; Ensure that non-compiled versions of everything are loaded.
;;
;; This file is part of Proof General.

;; Portions © Copyright 1994-2012, David Aspinall and University of Edinburgh
;; Portions © Copyright 1985-2014, Free Software Foundation, Inc
;; Portions © Copyright 2001-2006, Pierre Courtieu
;; Portions © Copyright 2010, Erik Martin-Dorel and École Normale Supérieure de Lyon
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

(setq load-path
      (append '("../generic/") load-path))
(load "proof-site.el")
(require 'proof-autoloads)
(require 'proof-compat)
(require 'proof-utils)


;; Prevent loading some files normally loaded in compilation


;; NB: Loading several prover files at once is a bit of a problem
;; with new config mechanism.
;; Could abstract more code in proof-site.el to avoid duplication here.
(let ((assistants (mapcar (function car) proof-assistant-table)))
					; assume not customized
  (while assistants
    (let*  
	((assistant (car assistants))	; compiler bogus warning here
	 (nameregexp			
	  (or 
	   (cdr-safe 
	    (assoc assistant
		   proof-assistant-table))
	   (error "proof-site: symbol " (symbol-name assistant) 
		  "is not in proof-assistant-table")))
	 (assistant-name (car nameregexp))
	 (sname		 (symbol-name assistant))
	 (elisp-file   sname))
      (setq proof-assistant-symbol nil)
      (proof-ready-for-assistant assistant assistant-name)
      ;; Must load proof-config each time to define proof assistant
      ;; specific variables
      (setq features (delete 'pg-custom features))
      (load "pg-custom.el")
      (load-library elisp-file)
      (setq assistants (cdr assistants)))))

;; Now a fake proof assistant to document the automatically
;; specific variables
(setq proof-assistant-symbol nil)
(proof-ready-for-assistant 'PA "PROOF ASSISTANT")
(setq features (delete 'pg-custom features))
(load "pg-custom.el")

;; These next few are autoloaded usually
(load "proof-menu.el")
(load "proof-toolbar.el")
(load "proof-script.el")
(load "proof-shell.el")
(load "pg-user.el")
(load "pg-goals.el")
(load "pg-response.el")
(load "unicode-tokens.el")
(load "proof-splash.el")

;; A couple of comint symbols are mentioned in the docs
(require 'comint)
;; Also completion
(require 'completion)

;; Set some symbols to make markup happen
(setq sml-mode 'markup-hack)
(setq func-menu 'markup-hack)

(load "texi-docstring-magic.el")
