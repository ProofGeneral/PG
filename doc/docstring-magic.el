;;; doc/docstring-magic.el --- hack for using texi-docstring-magic.

;; This file is part of Proof General.

;; Portions © Copyright 1994-2012  David Aspinall and University of Edinburgh
;; Portions © Copyright 2003-2018  Free Software Foundation, Inc.
;; Portions © Copyright 2001-2017  Pierre Courtieu
;; Portions © Copyright 2010, 2016  Erik Martin-Dorel
;; Portions © Copyright 2011-2013, 2016-2017  Hendrik Tews
;; Portions © Copyright 2015-2017  Clément Pit-Claudel

;; Author: David Aspinall
;; Maintainer:  Proof General maintainer <proofgen@dcs.ed.ac.uk>

;;; Commentary:
;;
;; Ensure that non-compiled versions of everything are loaded.
;;

;;; Code:

(setq load-path
      (append '("../generic/") load-path))
(load "proof-site.el")
(require 'proof-autoloads)
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
	   (error "Symbol %s is not in proof-assistant-table (in docstring-magic)"
                  (symbol-name assistant))))
	 (assistant-name (car nameregexp))
	 (sname		 (symbol-name assistant))
	 (elisp-file   sname))
      (setq proof-assistant-symbol nil)

      ;; proof-ready-for-assistant below loads pg-custom by require.
      ;; pg-custom must be loaded for each proof assistant again,
      ;; because it defines a lot with defpgcustom, which defines
      ;; assistent specific variables by using assistant-symbol
      ;; (called assistant here). Therefore, delete pg-custom from the
      ;; featue list here, such that it is also loaded for the second
      ;; proof assistant. Otherwise (proof-ass keymap) would fail
      ;; because the symbol is not defined for the second proof
      ;; assistant. And (proof-ass keymap) is put into
      ;; proof-ready-for-assistant-hook by
      ;; proof-eval-when-ready-for-assistant, when proof-script is
      ;; loaded as autoload for proof-ready-for-assistant, because
      ;; then proof-assistant is not yet set.
      (setq features (delete 'pg-custom features))
      (proof-ready-for-assistant assistant assistant-name)
      ;; Must load proof-config each time to define proof assistant
      ;; specific variables
      (setq features (delete 'pg-custom features))
      (load-library elisp-file)
      (setq assistants (cdr assistants)))))

;; Now a fake proof assistant to document the automatically
;; specific variables
(setq proof-assistant-symbol nil)

;; ensure again, pg-custom is loaded inside proof-ready-for-assistant,
;; see above
(setq features (delete 'pg-custom features))
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

(provide 'docstring-magic)
;;; docstring-magic.el ends here
