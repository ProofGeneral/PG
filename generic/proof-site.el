;;; proof-site.el --- Loading stubs for Proof General  -*- lexical-binding:t -*-

;; This file is part of Proof General.

;; Portions © Copyright 1994-2012  David Aspinall and University of Edinburgh
;; Portions © Copyright 2003-2021  Free Software Foundation, Inc.
;; Portions © Copyright 2001-2017  Pierre Courtieu
;; Portions © Copyright 2010, 2016  Erik Martin-Dorel
;; Portions © Copyright 2011-2013, 2016-2017  Hendrik Tews
;; Portions © Copyright 2015-2017  Clément Pit-Claudel

;; Author:      David Aspinall <David.Aspinall@ed.ac.uk>

;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:
;;
;; Loading stubs and configuration for site and choice of provers.
;;
;; NB: Normally users do not need to edit this file.  Developers/installers
;; may want to adjust proof-assistant-table-default below.
;;
;; The environment variables PROOFGENERAL_HOME and PROOFGENERAL_ASSISTANTS
;; can be set to affect load behaviour; see info documentation.
;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Master table of supported proof assistants.
;;

;;; Code:

;; Entries in proof-assistant-table-default are lists of the form
;;
;;   (SYMBOL NAME FILE-EXTENSION [AUTOMODE-REGEXP] [IGNORED-EXTENSIONS-LIST])
;;
;; FILE-EXTENSION is without dot ".". AUTOMODE-REGEXP is put into
;; auto-mode-alist, if it is not present, a regexp will be made up from
;; FILE-EXTENSION. IGNORED-EXTENSIONS-LIST, if present, is appended to
;; completion-ignored-extensions. See proof-assistant-table for more info.
;;
(defconst proof-assistant-table-default
    '(
      ;; Main instances of PG.

      (coq "Coq" "v" nil (".vo" ".glob" ".vok" ".vos"))
      (easycrypt "EasyCrypt" "ec" "\\.eca?\\'")
      (phox "PhoX" "phx" nil (".phi" ".pho"))
      (qrhl "qRHL" "qrhl")

      ;; Cut-and-paste management only

      (pgshell	 "PG-Shell" "pgsh")
      (pgocaml	 "PG-OCaml" "pgml")
      (pghaskell "PG-Haskell" "pghci")

      ;; Incomplete/obsolete:

      ;; (demoisa "Isabelle Demo" "ML")  ; obsolete
      )
    "Default value for `proof-assistant-table', which see.")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; PG version
;;

(eval-and-compile
  ;; WARNING: do not edit next line (constant is edited in Makefile.devel)
  (defconst proof-general-version "Proof General Version 4.6-git."
    "Version string identifying Proof General release."))

(defconst proof-general-short-version
  (eval-when-compile
    (progn
      (string-match "Version \\([^ ]+\\)\\." proof-general-version)
      (match-string 1 proof-general-version))))

(defconst proof-general-version-year "2022")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Top-level customization groups
;;

(defgroup proof-general nil
  "Customization of Proof General."
  :group 'applications
  :prefix "proof-")

(defgroup proof-general-internals nil
  "Customization of Proof General internals for proof assistant configuration."
  :group 'applications
  :group 'proof-general
  :prefix "proof-")


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Directories. Set at load time so compiled files can be relocated.
;; Load path must be extended manually during compilation.
;;

(defconst proof-home-directory
  (let ((curfile
	 (or
	  (and load-in-progress load-file-name)
	  buffer-file-name)))
    (if curfile
        (file-name-directory (directory-file-name
                              (file-name-directory curfile)))
      (let ((s (getenv "PROOFGENERAL_HOME")))
	(if s (file-name-as-directory s)))))
  "Directory where Proof General is installed.
Based on where the file `proof-site.el' was loaded from.
Falls back to consulting the environment variable `PROOFGENERAL_HOME' if
proof-site.el couldn't know where it was executed from.")

(defcustom proof-images-directory
  (expand-file-name "images/" proof-home-directory)
    "Where Proof General image files are installed.  Ends with slash."
  :type 'directory
  :group 'proof-general-internals)

(defcustom proof-info-directory
  (expand-file-name "doc/" proof-home-directory)
  "Where Proof General Info files are installed.  Ends with slash."
  :type 'directory
  :group 'proof-general-internals)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; load path. Have one function that adds elements to load-path.
;; Distributions having specific requirements (such as using
;; debian-pkg-add-load-path-item on Debian) only need to change
;; this function.
;;

(defun proof-add-to-load-path (dir)
  "Add DIR to `load-path' if not contained already."
  (add-to-list 'load-path dir))

(proof-add-to-load-path (expand-file-name "generic/" proof-home-directory))
(proof-add-to-load-path (expand-file-name "lib/" proof-home-directory))


;; Declare some global variables and autoloads

;; FIXME: Many of the autoloaded functions in there are internal to PG, and
;; are useless until PG is loaded, so they shouldn't be defined just because
;; proof-site is loaded!
(require 'proof-autoloads)

(defvar Info-dir-contents)

;; Add the info directory to the Info path
(if (file-exists-p proof-info-directory) ; for safety
    (if (and (boundp 'Info-directory-list) Info-directory-list)
	;; Info is already initialized.  Update its variables.
	(progn
	  (add-to-list 'Info-directory-list proof-info-directory)
	  (setq Info-dir-contents nil))
      ;; Info is not yet initialized.  Change its default.
      (add-to-list 'Info-default-directory-list proof-info-directory)))

(defcustom proof-assistant-table
  (apply
   #'append
   (mapcar
    ;; Discard entries whose directories have been removed.
    (lambda (dne)
      (let ((atts (file-attributes (expand-file-name (symbol-name (car dne))
					             proof-home-directory))))
	(if (and atts (eq 't (car atts)))
	    (list dne)
	  nil)))
    proof-assistant-table-default))
  "Proof General's table of supported proof assistants.
This is copied from `proof-assistant-table-default' at load time,
removing any entries that do not have a corresponding directory
under `proof-home-directory'.

Each entry is a list of the form

  (SYMBOL NAME FILE-EXTENSION [AUTOMODE-REGEXP] [IGNORED-EXTENSIONS-LIST])

The NAME is a string, naming the proof assistant.
The SYMBOL is used to form the name of the mode for the
assistant, `SYMBOL-mode', run when files with AUTOMODE-REGEXP
\(or with extension FILE-EXTENSION) are visited.  If present,
IGNORED-EXTENSIONS-LIST is a list of file-name extensions to be
ignored when doing file-name completion (IGNORED-EXTENSIONS-LIST
is added to `completion-ignored-extensions').

SYMBOL is also used to form the name of the directory and elisp
file for the mode, which will be

    PROOF-HOME-DIRECTORY/SYMBOL/SYMBOL.el

where PROOF-HOME-DIRECTORY is the value of the
variable `proof-home-directory'."
  ;; FIXME: make the last two elements optional in the type
  :type '(repeat (list symbol string regexp string))
  :group 'proof-general-internals)


(defcustom proof-assistants nil
  (concat
   "Choice of proof assistants to use with Proof General.
A list of symbols chosen from: "
   (mapconcat (lambda (astnt)
		(concat "`" (symbol-name (car astnt)) "'"))
	      proof-assistant-table
	      " ")
".\nIf nil, the default will be ALL available proof assistants.

Each proof assistant defines its own instance of Proof General,
providing session control, script management, etc.  Proof General
will be started automatically for the assistants chosen here.
To avoid accidently invoking a proof assistant you don't have,
only select the proof assistants you (or your site) may need.

You can select which proof assistants you want by setting this
variable before `proof-site.el' is loaded, or by setting
the environment variable `PROOFGENERAL_ASSISTANTS' to the
symbols you want, for example \"coq easycrypt\".  Or you can
edit the file `proof-site.el' itself.

Note: to change proof assistant, you must start a new Emacs session.")
  :type (cons 'set
	      (mapcar (lambda (astnt)
			(list 'const ':tag (nth 1 astnt) (nth 0 astnt)))
		      proof-assistant-table))
  :group 'proof-general)

(defvar proof-general-configured-provers
  (or (mapcar #'intern (split-string
                        (or (getenv "PROOFGENERAL_ASSISTANTS") "")))
      proof-assistants
      (mapcar #'car proof-assistant-table))
  "A list of the configured proof assistants.
Set on startup to contents of environment variable PROOFGENERAL_ASSISTANTS, the
Lisp variable `proof-assistants', or the contents of `proof-assistant-table'.")

;; Add auto-loads and load-path elements to support the
;; proof assistants selected, and define stub major mode functions
(let ((assistants proof-general-configured-provers))
  (while assistants
    (let*
	((assistant (car assistants))	; compiler bogus warning here
	 (tableentry
	  (or (assoc assistant
		     proof-assistant-table)
	      (error "Symbol %s is not in proof-assistant-table (in proof-site)"
		     (symbol-name assistant))))
	 (assistant-name (nth 1 tableentry))
	 (regexp	 (or (nth 3 tableentry)
			     (concat (regexp-quote ".")
				     (regexp-quote (nth 2 tableentry))
				     "\\'")))
	 (sname		 (symbol-name assistant))
	 ;; NB: File name for each prover is the same as its symbol name!
	 (elisp-file   sname)
	 ;; NB: Mode name for each prover is <symbol name>-mode!
	 (proofgen-mode  (intern (concat sname "-mode")))
	 ;; NB: Customization group for each prover is its l.c.'d name!

	 ;; Stub to initialize and load specific code.
	 (mode-stub
	  ;; FIXME: Make it a closure with (:documentation EXP)
          ;; once we don't need compatibility with Emacs<25.
	  `(lambda ()
	     ,(concat
	       "Major mode for editing scripts for proof assistant "
	       assistant-name
	       ".\nThis is a stub which loads the real function.")
	     (interactive)
	     ;; Stop loading if proof-assistant is already set:
	     ;; cannot work for more than one prover.
	     (cond
	      ((and (boundp 'proof-assistant)
		    (not (string-equal proof-assistant "")))
	       (or (string-equal proof-assistant ,assistant-name)
		   ;; If Proof General was partially loaded last time
		   ;; and mode function wasn't redefined, be silent.
		   (message
		    (concat
		     ,assistant-name
		     " Proof General error: Proof General already in use for "
		     proof-assistant))))
	      (t
	       ;; prepare variables and load path
	       (proof-ready-for-assistant (quote ,assistant) ,assistant-name)
	       ;; load the real mode and invoke it.
	       (load-library ,elisp-file)
	       (,proofgen-mode))))))

	(add-to-list 'auto-mode-alist (cons regexp proofgen-mode))

        (unless (fboundp proofgen-mode)
	  (fset proofgen-mode mode-stub))

	(dolist (ext (nth 4 tableentry))
	  (add-to-list 'completion-ignored-extensions ext)
          (when (boundp 'dired-omit-extensions)
	    (add-to-list 'dired-omit-extensions ext)))

	(setq assistants (cdr assistants)))))

;;
;; Easy entry points
;;

(defun proof-chose-prover (prompt)
  (completing-read prompt
		   (mapcar #'symbol-name
			   proof-general-configured-provers)))

(defun proofgeneral (prover)
  "Start proof general for prover PROVER."
  (interactive
   (list (proof-chose-prover "Start Proof General for theorem prover: ")))
  (proof-ready-for-assistant (intern prover)
			     (nth 1 (assoc (intern prover)
					   proof-assistant-table-default)))
  (require (intern prover)))

(defun proof-visit-example-file (prover)
  "Visit a standardly named example file for prover PROVER."
  (interactive
   (list (proof-chose-prover "Visit example file for prover: ")))
  (find-file (expand-file-name
	      (concat prover "/example."
		      (nth 2 (assoc (intern prover)
		                    proof-assistant-table-default)))
	      proof-home-directory)))




(provide 'proof-site)

;;; proof-site.el ends here
