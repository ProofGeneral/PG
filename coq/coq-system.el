;;; coq-system.el --- common part of compilation feature  -*- lexical-binding:t -*-

;; This file is part of Proof General.

;; Portions © Copyright 1994-2012  David Aspinall and University of Edinburgh
;; Portions © Copyright 2003-2021  Free Software Foundation, Inc.
;; Portions © Copyright 2001-2017  Pierre Courtieu
;; Portions © Copyright 2010, 2016  Erik Martin-Dorel
;; Portions © Copyright 2011-2013, 2016-2017  Hendrik Tews
;; Portions © Copyright 2015-2017  Clément Pit-Claudel

;; Authors: Hendrik Tews, Pierre Courtieu
;; Maintainer: Pierre.Courtieu<Pierre.Courtieu@cnam.fr>

;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:
;;
;; This file holds constants, options and some general functions for
;; setting Coq command arguments.  Some code is dedicated as a light
;; support for older versions of Coq.
;;

;;; Code:

(require 'cl-lib)
(require 'proof)
(require 'coq-mode)                     ;for coq-prog-name
(require 'coq-project)                  ;load path and project file

;; Here, not in coq-project.el, so that the uses below compile without
;; warning.
(make-obsolete-variable 'coq-load-path-include-current "Coq 8.5 does not need it" "4.3")

(defvar coq-prog-args)
(defvar coq-debug)

;; Regular files should be of the form "valid_modulename.v" coq
;; accepts lots of things as letter, this is probably much stricter.
;; In practice it should be OK though, except maybe for non latin
;; characters.
(defun coq-regular-filename-p (s)
  (let ((noext (file-name-base s)))
    (string-match-p "\\`[[:alpha:]_][[:alnum:]_]*\\'" noext)))

;; DEAD CODE?
(defcustom coq-prog-env nil
  "List of environment settings to pass to Coq process.
On Windows you might need something like:
  (setq coq-prog-env \\='(\"HOME=C:\\Program Files\\Coq\\\"))"
  :group 'coq)

;; We just call "rocq" and look the error message that should mention
;; subcommands
(defun coq-detect-rocq-cli (&optional coq-command)
  "Return non nil if the detected Coq/Rocq executable obeys the Rocq CLI.
If COQ-COMMAND is non nil, test this executable instead."
  (let* ((coq-command (or coq-command proof-prog-name (coq-autodetect-progname))))
    (condition-case nil
        (string-match
         "Supported subcommands:"
         (with-output-to-string (process-file coq-command nil standard-output)))
      (error nil))))

(defun coq-detect-coqdep ()
  (if (coq-detect-rocq-cli) (coq-detect-prog-gen "rocq")
    (coq-detect-prog-gen "coqdep")))
(defun coq-detect-coqc ()
  (if (coq-detect-rocq-cli) (coq-detect-prog-gen "rocq")
    (coq-detect-prog-gen "coqc")))

;; We return the executable coqtop or rocq. But not the rocq option
;; (dep, c, top)) triggering the particular tool.
(defcustom coq-dependency-analyzer (coq-detect-coqdep)
  "Command to invoke coqdep."
  :type 'string
  :safe 'stringp
  :group 'coq)

(defcustom coq-compiler (coq-detect-coqc)
   ;FIXME
  "Command to invoke the Coq compiler."
  :type 'string
  :safe 'stringp
  :group 'coq)

(defcustom coq-pinned-version nil
  "Manual Coq version override (rarely needed).
There should be no need to set this value unless you use old trunk versions from
the Coq GitHub repository.  For Coq versions with decent version numbers
Proof General detects the version automatically and adjusts itself.  This
variable should contain nil or a version string."
  :type 'string
  :group 'coq)

(defvar coq-autodetected-version nil
  "Version of Coq, as autodetected by `coq-autodetect-version'.")

(defvar coq-autodetected-help nil
  "Options of Coq, as autodetected by `coq-autodetect-options'.
This variable contains the output of \"coqtop -help\"")

;;; error symbols

;; coq-unclassifiable-version
;;
;; This error is signaled with one data item -- the bad version string

(put 'coq-unclassifiable-version 'error-conditions
     '(error coq-unclassifiable-version))

(put 'coq-unclassifiable-version 'error-message
     "Proof General cannot classify your Coq version")


;;; version detection code

(defun coq-version (&optional may-recompute)
  "Return the precomputed version of the current Coq toolchain.
With MAY-RECOMPUTE, try auto-detecting it if it isn't available."
  (or coq-pinned-version
      coq-autodetected-version
      (when may-recompute
        (coq-autodetect-version))))

(defun coq-show-version ()
  "Show the version of Coq currently in use.
If it doesn't look right, try `coq-autodetect-version'."
  (interactive)
  (let ((version (coq-version nil)))
    (if version
        (message "Using Coq v%s" coq-autodetected-version)
      (message "Coq version unknown at this time. Use `coq-autodetect-version' to autodetect."))))

(defun coq-callcoq (option &optional expectedretv)
  "Call coqtop with the given OPTION and return the output.
The given option should make coqtop return immediately.
Optionally check the return code and return nil if the check
fails.  Return also nil on other kinds of errors (e.g., `coq-prog-name'
not found).
This function supports calling coqtop via tramp.
This function must not rely on coq-autodetect-version, it would be a cycle."
  (let* ((coq-command (or proof-prog-name coq-prog-name (coq-autodetect-progname)))
         ;; test the executable we call, not another one
         (coq-args (if (coq-detect-rocq-cli coq-command) (list "top" option) (list option)))
         retv)
    (condition-case nil
        ;; Fail gracefully if rocq/coq executable not found.  No
        ;; `with-temp-buffer' here: the process must run in the
        ;; environment of the current buffer (e.g., for direnv).
        (let ((output (with-output-to-string
                        (setq retv (apply 'process-file coq-command nil
                                          standard-output nil coq-args)))))
          (if (or (not expectedretv) (equal retv expectedretv)) output))
      (error
       (message "Warning: No Coq/Rocq version detected yet (tried \"%s\")." coq-command)
       (message "ProofGeneral will not be able to start a prover session until you have one in your path.")
       nil))))

(defun coq-autodetect-version (&optional interactive-p)
  "Detect and record the version of Coq currently in use.
Interactively (with INTERACTIVE-P), show that number."
  (interactive '(t))
  (setq coq-autodetected-version nil)
  (let* ((str (coq-callcoq "-v" 0))
         (mtch (and str (string-match "version \\([^ \r\n]+\\)" str))))
    (when mtch
      (setq coq-autodetected-version (match-string 1 str))))
  (when interactive-p (coq-show-version))
  coq-autodetected-version)

(defun coq-autodetect-help (&optional _interactive-p)
  "Record the output of coqtop -help in `coq-autodetected-help'."
  (interactive '(t))
  (let ((coq-output (coq-callcoq "-help")))
    (setq coq-autodetected-help (or coq-output ""))))

(defun coq--version< (v1 v2)
  "Compare Coq versions V1 and V2.
If V1 or V2 is not a valid version number, return nil."
  ;; -snapshot is only supported by Emacs 24.5, not 24.3
  (let ((version-regexp-alist `(("^[-_+ ]?snapshot$" . -4)
                                ("^pl$" . 0)
                                ,@version-regexp-alist)))
    (condition-case nil (version< v1 v2) (error nil))))

(defcustom coq-pre-v85 nil
  "Deprecated.
Use `coq-pinned-version' if you want to bypass the
version detection that Proof General does automatically."
  :type 'boolean
  :group 'coq)

(defun coq--pre-v85 ()
  "Return non-nil if the auto-detected version of Coq is < 8.5.
Returns nil if the version can't be detected."
  (let ((coq-version-to-use (or (coq-version t) "8.5")))
    (condition-case err
	(coq--version< coq-version-to-use "8.5snapshot")
      (error
       (cond
	((equal (substring (cadr err) 0 15) "Invalid version")
	 (signal 'coq-unclassifiable-version  coq-version-to-use))
	(t (signal (car err) (cdr err))))))))

(defun coq--post-v86 ()
  "Return t if the auto-detected version of Coq is >= 8.6.
Return nil if the version cannot be detected."
  (let ((coq-version-to-use (or (coq-version t) "8.5")))
    (condition-case err
	(not (coq--version< coq-version-to-use "8.6"))
      (error
       (cond
	((equal (substring (cadr err) 0 15) "Invalid version")
	 (signal 'coq-unclassifiable-version  coq-version-to-use))
	(t (signal (car err) (cdr err))))))))

(defun coq--post-v809 ()
  "Return t if the auto-detected version of Coq is >= 8.9.
Return nil if the version cannot be detected."
  (let ((coq-version-to-use (or (coq-version t) "8.8")))
    (condition-case err
	(not (coq--version< coq-version-to-use "8.10alpha"))
      (error
       (cond
	((equal (substring (cadr err) 0 15) "Invalid version")
	 (signal 'coq-unclassifiable-version  coq-version-to-use))
	(t (signal (car err) (cdr err))))))))

(defun coq--post-v810 ()
  "Return t if the auto-detected version of Coq is >= 8.10.
Return nil if the version cannot be detected."
  (let ((coq-version-to-use (or (coq-version t) "8.9")))
    (condition-case err
	(not (coq--version< coq-version-to-use "8.10alpha"))
      (error
       (cond
	((equal (substring (cadr err) 0 15) "Invalid version")
	 (signal 'coq-unclassifiable-version  coq-version-to-use))
	(t (signal (car err) (cdr err))))))))

(defun coq--post-v811 ()
  "Return t if the auto-detected version of Coq is >= 8.11.
Return nil if the version cannot be detected."
  (let ((coq-version-to-use (or (coq-version t) "8.10")))
    (condition-case err
	(not (coq--version< coq-version-to-use "8.11"))
      (error
       (cond
	((equal (substring (cadr err) 0 15) "Invalid version")
	 (signal 'coq-unclassifiable-version  coq-version-to-use))
	(t (signal (car err) (cdr err))))))))

(defun coq--post-v814 ()
  "Return t if the auto-detected version of Coq is >= 8.14.
Return nil if the version cannot be detected."
  (let ((coq-version-to-use (or (coq-version t) "8.13")))
    (condition-case err
	(not (coq--version< coq-version-to-use "8.14"))
      (error
       (cond
	((equal (substring (cadr err) 0 15) "Invalid version")
	 (signal 'coq-unclassifiable-version  coq-version-to-use))
	(t (signal (car err) (cdr err))))))))

(defun coq--pre-v816 ()
  "Return t if the auto-detected version of Coq is < 8.16.
Return nil if the version cannot be detected."
  (let ((coq-version-to-use (or (coq-version t) "8.16")))
    (condition-case err
	(coq--version< coq-version-to-use "8.16")
      (error
       (cond
	((equal (substring (cadr err) 0 15) "Invalid version")
	 (signal 'coq-unclassifiable-version  coq-version-to-use))
	(t (signal (car err) (cdr err))))))))

(defun coq--is-v817 ()
  "Return t if the auto-detected version of Coq is some 8.17 version.
Return nil if the version cannot be detected."
  (let ((coq-version-to-use (or (coq-version t) "8.16")))
    (condition-case err
	(and  (not (coq--version< coq-version-to-use "8.17"))
              (coq--version< coq-version-to-use "8.18alpha"))
      (error
       (cond
	((equal (substring (cadr err) 0 15) "Invalid version")
	 (signal 'coq-unclassifiable-version  coq-version-to-use))
	(t (signal (car err) (cdr err))))))))

(defun coq--post-v818 ()
  "Return t if the auto-detected version of Coq is >= 8.19alpha.
Return nil if the version cannot be detected."
  (let ((coq-version-to-use (or (coq-version t) "8.18")))
    (condition-case err
	(not (coq--version< coq-version-to-use "8.19alpha"))
      (error
       (cond
	((equal (substring (cadr err) 0 15) "Invalid version")
	 (signal 'coq-unclassifiable-version  coq-version-to-use))
	(t (signal (car err) (cdr err))))))))

(defun coq--post-v92 ()
  "Return t if the auto-detected version of Coq is >= 9.2+alpha.
Return nil if the version cannot be detected."
  (let ((coq-version-to-use (or (coq-version t) "9.1")))
    (condition-case err
	(not (coq--version< coq-version-to-use "9.2+alpha"))
      (error
       (cond
	((equal (substring (cadr err) 0 15) "Invalid version")
	 (signal 'coq-unclassifiable-version  coq-version-to-use))
	(t (signal (car err) (cdr err))))))))

(defun coq--supports-topfile ()
  "Return t if \"-topfile\" appears in coqtop options."
  (string-match "-topfile" coq-autodetected-help)
)

(defcustom coq-use-makefile nil
  "Whether to look for a Makefile to attempt to guess the command line.
Set to t if you want this feature, but note that it is deprecated."
  :type 'string
  :group 'coq)

(defcustom coq-www-home-page "http://coq.inria.fr/"
  "Coq home page URL."
  :type 'string
  :group 'coq)

(defun coq--options-test-roundtrip-1 (coq-project parsed pre-v85)
  "Run a sanity check on COQ-PROJECT's PARSED options.
If PRE-V85 is non-nil, use compatibility mode."
  (let* ((concatenated (apply #'append parsed))
         (coq-load-path-include-current nil)
         (extracted (coq--extract-load-path parsed nil))
         (roundtrip (coq-include-options extracted nil pre-v85)))
    (princ (format "[%s] with compatibility flag set to %S: " coq-project pre-v85))
    (if (equal concatenated roundtrip)
        (princ "OK\n")
      (princ (format "Failed.\n:: Original:  %S\n:: LoadPath: %S\n:: Roundtrip: %S\n"
                     concatenated extracted roundtrip)))))

(defun coq--options-test-roundtrip (coq-project &optional v85-only)
  "Run a sanity check on COQ-PROJECT.
If V85-ONLY is non-nil, do not check the compatibility code."
  (let ((parsed (coq--read-options-from-project-file coq-project)))
    (coq--options-test-roundtrip-1 coq-project parsed nil)
    (unless v85-only
      (coq--options-test-roundtrip-1 coq-project parsed t))))

(defun coq--options-test-roundtrips ()
  "Run sanity tests on coq-project parsing code.
More precisely, check that parsing and reprinting the include
options of a few coq-project files does the right thing."
  (with-output-to-temp-buffer "*tests*"
    (coq--options-test-roundtrip "-Q /test Test")
    (coq--options-test-roundtrip "-Q /test \"\"")
    (coq--options-test-roundtrip "-R /test Test")
    (coq--options-test-roundtrip "-I /test")))

;; it is better to inform coqtop of the name of the current module
;; using the -topfile option, so that coqtop understands references
;; to it. But don't insist if this would fail (because of wrong
;; file name): Some people want to script .v files without ever
;; compiling them.
(defun coq-topfile-prog-arg ()
  (when (and (coq--supports-topfile) buffer-file-name)
    (if (coq-regular-filename-p buffer-file-name)
        (cons "-topfile" (cons buffer-file-name nil))
      (message "Warning: buffer %s seems not compilable," buffer-file-name)
      (message "probably because of its name, no -topfile option set.")
      nil)))


(defun coq-include-prog-args (loadpath &optional current-directory pre-v85)
  "Build a list of options for coqdep.
LOADPATH, CURRENT-DIRECTORY, PRE-V85: see `coq-include-options'."
  (coq-include-options loadpath current-directory pre-v85))

(defun coq--clean-prog-args (args)
  "Return ARGS without the entries added by `coq-coqtop-prog-args' and such.

Such entries are currently -emacs and -topfile."
  (pcase args
    ((or `("-emacs" . ,rest) `("top" . ,rest) `("c" . ,rest) `("dep" . ,rest)
         `("-topfile" . (,(pred (apply-partially #'equal buffer-file-name)) . ,rest)))
     (coq--clean-prog-args rest))
    (`(,car . ,cdr)
     (cons car (coq--clean-prog-args cdr)))
    (_ args)))


(defun coq-common-prog-args (loadpath &optional current-directory pre-v85)
  "Build a list of options common for coqc and coqtop and coqdep.
LOADPATH, CURRENT-DIRECTORY, PRE-V85: see `coq-include-options'."
;; coqtop always adds the current directory to the LoadPath, so don't
  ;; include it in the -Q options.
  (append (coq--clean-prog-args coq-prog-args)
          (let* ((coq-load-path-include-current nil)
                 (cmd (coq-include-prog-args loadpath current-directory pre-v85)))
            cmd)))

(defun coq-coqdep-prog-args (loadpath &optional current-directory pre-v85)
  "Build a list of option for coqdep."
  (append
   (if (coq-detect-rocq-cli) (list "dep") nil)
   (coq-include-prog-args loadpath current-directory pre-v85)))

(defun coq-coqc-prog-args (loadpath &optional current-directory pre-v85)
  "Build a list of options for coqc.
LOADPATH, CURRENT-DIRECTORY, PRE-V85: see `coq-include-options'."
  ;; coqtop always adds the current directory to the LoadPath, so don't
  ;; include it in the -Q options.
  (append (if (coq-detect-rocq-cli) (list "c") nil)
          (coq-common-prog-args loadpath current-directory pre-v85)))

;; coq-coqtop-prog-args is the user-set list of arguments to pass to
;; Coq process, see 'defpacustom prog-args' in pg-custom.el for
;; documentation.

(defun coq-coqtop-prog-args (loadpath &optional current-directory pre-v85)
  ;; coqtop always adds the current directory to the LoadPath, so don't
  ;; include it in the -Q options. This is not true for coqdep.
  "Build a list of options for coqc.
LOADPATH, CURRENT-DIRECTORY, PRE-V85: see `coq-coqc-prog-args'."
  (append
   (if (coq-detect-rocq-cli) (list "top") nil)
   (list "-emacs")
   (coq-topfile-prog-arg)
   (coq-common-prog-args loadpath current-directory pre-v85)))

(defun coq-prog-args ()
  "Recompute `coq-load-path' before calling `coq-coqtop-prog-args'."
  (coq-load-project-file)
  (coq-autodetect-version)
  (coq-autodetect-help)
  (coq-coqtop-prog-args coq-load-path))

;; Detecting coqtop or rocq args should happen at the last moment
;; before calling the process. In particular it should happen after
;; that proof-prog-name-ask is performed. This hook is exactly called
;; after querying proof-prog-name (if proof-prog-name-ask is t). This
;; way the options can be elaborated correctly (coqtop and rocq have
;; different options). We first detect executables (taking
;; proof-prog-name-ask into account), then elaborate the
;; proof-prog-args. Note: we don't support proof-prog-name-guess
(defun coq--user-set-p (symbol)
  "Return non-nil if the user set option SYMBOL with customize."
  (or (get symbol 'saved-value) (get symbol 'customized-value)))

(add-hook 'proof-shell-before-process-hook
          (lambda ()
            ;; detect executables unless explicitely set by hand,
            ;; with proof-prog-name-ask or by customizing them. When
            ;; the prover is set by hand, we try to find coqdep and
            ;; coqc in the same directory
            (if (and (or proof-prog-name-ask (coq--user-set-p 'coq-prog-name))
                     proof-prog-name)
                ;; let us find other executables in the exact same
                ;; place. TODO: go back to default exec-path if not found?
                (let ((exec-path (if (file-name-directory proof-prog-name)
                                     (list (file-name-directory proof-prog-name))
                                   exec-path)))
                  (unless (coq--user-set-p 'coq-compiler)
                    (setq coq-compiler (executable-find (coq-detect-coqc))))
                  (unless (coq--user-set-p 'coq-dependency-analyzer)
                    (setq coq-dependency-analyzer (executable-find (coq-detect-coqdep)))))
              ;; default: detect everything from the current exec-path
              (setq proof-prog-name (coq-autodetect-progname))
              (unless (coq--user-set-p 'coq-compiler)
                (setq coq-compiler (coq-detect-coqc)))
              (unless (coq--user-set-p 'coq-dependency-analyzer)
                (setq coq-dependency-analyzer (coq-detect-coqdep))))
            (when coq-debug
              (message "coq-proof-prog-name: %S" proof-prog-name)
              (message "coq-dependency-analyzer: %S"  coq-dependency-analyzer)
              (message "coq-compiler: %S"  coq-compiler))
            ;; in principe coq-prog-name is only used to set up
            ;; proof-prog-name, but it may sometimes be used by itself
            ;; so let us sync them
            (setq coq-prog-name proof-prog-name)
            ;; now elaborate args
            (setq coq-prog-args (coq-prog-args))))

;; smie's parenthesis blinking is too slow, let us have the default one back
(add-hook 'coq-mode-hook
          (lambda ()
            (when (and (fboundp 'show-paren--default)
                       (boundp 'show-paren-data-function))
              (setq show-paren-data-function #'show-paren--default))))


(provide 'coq-system)

;;; coq-system.el ends here
