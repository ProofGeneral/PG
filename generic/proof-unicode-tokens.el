;;; proof-unicode-tokens.el --- Support Unicode tokens package
;;
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
;;
;;; Commentary:
;;
;; Support for Unicode Tokens package: per-prover global enabling, copying
;; configuration tables, adding mode hooks to turn on/off.
;;
;; Initialisation:
;;   proof-unicode-tokens-init -> proof-unicode-tokens-reconfigure*
;;

;;; Code:

(eval-when-compile
  (require 'cl-lib))

(eval-when-compile
  (require 'proof-auxmodes)	 ; loaded by proof.el, autoloads us
  (require 'unicode-tokens))	 ; it will be loaded by proof-auxmodes

(require 'proof-config)			; config variables

(defvar proof-unicode-tokens-initialised nil
  "Flag indicating whether or not we've performed startup.")

(defun proof-unicode-tokens-init ()
  "Set tables and configure hooks for modes."
  (proof-unicode-tokens-configure)
  (let ((hooks (mapcar (lambda (m)
			 (intern (concat (symbol-name m) "-hook")))
		       (list
			proof-mode-for-script
			proof-mode-for-response
			proof-mode-for-goals))))
    (dolist (hook hooks)
      (add-hook hook 'proof-unicode-tokens-mode-if-enabled)))
  (setq proof-unicode-tokens-initialised t))

(defun proof-unicode-tokens-configure ()
  "Set the Unicode Tokens table from prover instances and initialise."
  (require 'unicode-tokens) ; load now, for unicode-tokens-configuration-variables
  (dolist (var unicode-tokens-configuration-variables)
    (if (boundp (proof-ass-symv var))
	(set (intern (concat "unicode-tokens-" (symbol-name var)
			     "-variable"))
	     (proof-ass-symv var))))
  (unicode-tokens-initialise))


;;;###autoload
(defun proof-unicode-tokens-mode-if-enabled ()
  "Turn on or off the Unicode Tokens minor mode in this buffer."
  (unicode-tokens-mode
   (if (proof-ass unicode-tokens-enable) 1 0)))

;;;###autoload
(defun proof-unicode-tokens-set-global (flag)
  "Set global status of unicode tokens mode for PG buffers to be FLAG.
Turn on/off menu in all script buffers and ensure new buffers follow suit."
  (unless proof-unicode-tokens-initialised
    (proof-unicode-tokens-init))
  ;; Configure if already running
  (proof-map-buffers
   (append
    (proof-buffers-in-mode proof-mode-for-script)
    (proof-associated-buffers)
    (proof-buffers-in-mode proof-tokens-extra-modes))
   (unicode-tokens-mode (if flag 1 0)))
  (proof-unicode-tokens-configure-prover))


;;;###autoload
(defun proof-unicode-tokens-enable ()
  "Turn on or off Unicode tokens mode in Proof General script buffer.
This invokes `unicode-tokens-mode' to toggle the setting for the current
buffer, and then sets PG's option for default to match.
Also we arrange to have unicode tokens mode turn itself on automatically
in future if we have just activated it for this buffer.
Note: this function is called when the customize setting for the prover
is changed."
  (interactive)
  (when (proof-unicode-tokens-support-available) ;; loads unicode-tokens
    (unless proof-unicode-tokens-initialised
      (proof-unicode-tokens-init))
    (with-no-warnings ; spurious warning on `proof-unicode-tokens-set-global'
      (proof-unicode-tokens-set-global (not unicode-tokens-mode)))))


;;;
;;; Interface to custom to dynamically change tables (via proof-set-value)
;;;

(defun proof-unicode-tokens-reconfigure ()
  "Function called after a token configuration is changed.
Switch off tokens in all script buffers, recalculate maps, turn on again."
  (interactive)
  (when proof-unicode-tokens-initialised ; not on startup
    (when (proof-ass unicode-tokens-enable)
      (proof-map-buffers
       (proof-buffers-in-mode proof-mode-for-script)
       (unicode-tokens-mode 0)))
    (proof-unicode-tokens-configure)
    (when (proof-ass unicode-tokens-enable)
      (proof-map-buffers
       (proof-buffers-in-mode proof-mode-for-script)
       (unicode-tokens-mode 1)))))

;; functions to dynamically change settings
(eval-after-load "unicode-tokens"
  '(dolist (var unicode-tokens-configuration-variables)
     (funcall 'defalias
	      (intern (concat "proof-" (symbol-name var)))
	      'proof-unicode-tokens-reconfigure)))

;;;
;;; Configuration interface to server
;;;

(defun proof-unicode-tokens-configure-prover ()
  (if (proof-ass unicode-tokens-enable)
      (proof-unicode-tokens-activate-prover)
    (proof-unicode-tokens-deactivate-prover)))

(defun proof-unicode-tokens-activate-prover ()
  (when proof-tokens-activate-command
    (proof-server-invisible-command-invisible-result
     proof-tokens-activate-command)))

(defun proof-unicode-tokens-deactivate-prover ()
  (when proof-tokens-deactivate-command
    (proof-server-invisible-command-invisible-result
     proof-tokens-deactivate-command)))

(provide 'proof-unicode-tokens)

;;; proof-unicode-tokens.el ends here
