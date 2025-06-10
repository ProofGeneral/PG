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

(require 'proof)
(require 'coq-mode)                     ;for coq-prog-name

(defvar coq-prog-args)
(defvar coq-debug)

;; Regular files should be of the form "valid_modulename.v" coq
;; accepts lots of things as letter, this is probably much stricter.
;; In practice it should be OK though, except maybe for non latin
;; characters.
(defun coq-regular-filename-p (s)
  (let ((noext (file-name-base s)))
    (string-match-p "\\`[[:alpha:]_][[:alnum:]_]*\\'" noext)))

;; Arbitrary arguments can already be passed through _CoqProject.
;; However this is not true for all assistants, so we don't modify the
;; (defpgcustom prog-args) declaration.
(defun coq--string-list-p (obj)
  "Determine if OBJ is a list of strings."
  (or (null obj) (and (consp obj) (stringp (car obj)) (coq--string-list-p (cdr obj)))))

(put 'coq-prog-args 'safe-local-variable #'coq--string-list-p)

;; DEAD CODE?
(defcustom coq-prog-env nil
  "List of environment settings to pass to Coq process.
On Windows you might need something like:
  (setq coq-prog-env '(\"HOME=C:\\Program Files\\Coq\\\"))"
  :group 'coq)

;; We just call "rocq" and look the error message that should mention
;; subcommands
(defun coq-detect-rocq-cli ()
  "Return non nil if the detected Coq/Rocq executable obeys the Rocq CLI."
  (let* ((coq-command (or proof-prog-name (coq-autodetect-progname))))
    (condition-case nil
        (with-temp-buffer
          (apply 'process-file (list coq-command nil t))
          (string-match "Supported subcommands:" (buffer-string)))
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
         (coq-args (if (coq-detect-rocq-cli) (list "top" option) (list option)))
         (process-args (append (list coq-command nil t nil) coq-args))
         retv)
    (condition-case nil
        (with-temp-buffer
          (setq retv (apply 'process-file process-args))
          (if (or (not expectedretv) (equal retv expectedretv)) (buffer-string)))
      (error nil))))

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
  "Compare Coq versions V1 and V2."
  ;; -snapshot is only supported by Emacs 24.5, not 24.3
  (let ((version-regexp-alist `(("^[-_+ ]?snapshot$" . -4)
                                ("^pl$" . 0)
                                ,@version-regexp-alist)))
    (version< v1 v2)))

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

;;; utility functions for variables

(defun coq-load-path-safep (path)
  "Check if PATH is a safe value for `coq-load-path'."
  (and
   (listp path)
   (cl-every
    (lambda (entry)
      (or (stringp entry)
          (and (listp entry)
               (eq (car entry) 'rec)
               (cl-every 'stringp (cdr entry))
               (equal (length entry) 3))
          (and (listp entry)
               (eq (car entry) 'nonrec)
               (cl-every 'stringp (cdr entry))
               (equal (length entry) 3))
          (and (listp entry)
               (eq (car entry) 'recnoimport)
               (cl-every 'stringp (cdr entry))
               (equal (length entry) 3))
          (and (listp entry)
               (eq (car entry) 'ocamlimport)
               (cl-every 'stringp (cdr entry))
               (equal (length entry) 2))
          (and (listp entry)
               (cl-every 'stringp entry)
               (equal (length entry) 2))))
    path)))

(defcustom coq-load-path nil
  "Non-standard Coq library load path.
This list specifies the LoadPath extension for coqdep, coqc and
coqtop.  Usually, the elements of this list are strings (for
\"-I\") or lists of two strings (for \"-R\" dir path and
\"-Q\" dir path).

The possible forms of elements of this list correspond to the 4
forms of include options (`-I' `-Q' and `-R').  An element can be

  - A list of the form `(\\='ocamlimport dir)', specifying (in 8.5) a
    directory to be added to Ocaml path (`-I').
  - A list of the form `(\\='rec dir path)' (where dir and path are
    strings) specifying a directory to be recursively mapped to the
    logical path `path' (`-R dir path').
  - A list of the form `(\\='recnoimport dir path)' (where dir and
    path are strings) specifying a directory to be recursively
    mapped to the logical path `path' (`-Q dir path'), but not
    imported (modules accessible for import with qualified names
    only).  Note that -Q dir \"\" has a special, nonrecursive meaning.
  - A list of the form (8.4 only) `(\\='nonrec dir path)', specifying a
    directory to be mapped to the logical path 'path' ('-I dir -as path').

For convenience the symbol `rec' can be omitted and entries of
the form `(dir path)' are interpreted as `(rec dir path)'.

A plain string maps to -Q ... \"\" in 8.5, and -I ... in 8.4.

Under normal circumstances this list does not need to
contain the Coq standard library or \".\" for the current
directory (see `coq-load-path-include-current').

WARNING: if you use Coq <= 8.4, the meaning of these options is
not the same (-I is for Coq path)."
  :type '(repeat (choice (string :tag "simple directory without path (-Q \"\") in 8.5, -I in 8.4")
                         (list :tag
                               "recursive directory with path (-R ... ...)"
                               (const rec)
                               (string :tag "directory")
                               (string :tag "log path"))
                         (list :tag
                               "recursive directory without recursive import with path (-Q ... ...)"
                               (const recnoimport)
                               (string :tag "directory")
                               (string :tag "log path"))
                         (list :tag
                               "compatibility for of -I (-I ... -as ... in coq<=8.4)"
                               (const nonrec)
                               (string :tag "directory")
                               (string :tag "log path"))
                         (list :tag
                               "ocaml path (-I ...)"
                               (const ocamlimport)
                               (string :tag "directory")
                               (string :tag "log path"))))
  :safe 'coq-load-path-safep
  :group 'coq-auto-compile)

(make-variable-buffer-local 'coq-load-path)

(defcustom coq-load-path-include-current t
  "If t, let coqdep search the current directory too.
Should be t for normal users.  If t, pass -Q dir \"\" to coqdep when
processing files in directory \"dir\" in addition to any entries in
`coq-load-path'.

This setting is only relevant with Coq < 8.5."
  :type 'boolean
  :safe 'booleanp
  :group 'coq-auto-compile)

(make-obsolete-variable 'coq-load-path-include-current "Coq 8.5 does not need it" "4.3")

(defun coq-option-of-load-path-entry (entry &optional pre-v85)
  "Translate a single ENTRY from `coq-load-path' into options.
See `coq-load-path' for the possible forms of ENTRY and to which
options they are translated.  Use a non-nil PRE-V85 flag to
request compatibility handling of flags."
  (if pre-v85
      ;; FIXME Which base directory do we expand against? Should the entries of
      ;; load-path just always be absolute?
      ;; NOTE we don't handle 'recnoimport in 8.4, and we don't handle 'nonrec
      ;; in 8.5.
      (pcase entry
        ((or (and (pred stringp) dir) `(ocamlimport ,dir))
         (list "-I" (expand-file-name dir)))
        (`(nonrec ,dir ,alias)
         (list "-I" (expand-file-name dir) "-as" alias))
        ((or `(rec ,dir ,alias) `(,dir ,alias))
         (list "-R" (expand-file-name dir) alias)))
    (pcase entry
      ((and (pred stringp) dir)
       (list "-Q" (expand-file-name dir) ""))
      (`(ocamlimport ,dir)
       (list "-I" (expand-file-name dir)))
      (`(recnoimport ,dir ,alias)
       (list "-Q" (expand-file-name dir) alias))
      ((or `(rec ,dir ,alias) `(,dir ,alias))
       (list "-R" (expand-file-name dir) alias)))))

(defun coq-include-options (loadpath &optional current-directory pre-v85)
  "Build the base list of include options for coqc, coqdep and coqtop.
The options list includes all entries from argument LOADPATH
\(which should be `coq-load-path' of the buffer that invoked the
compilation) prefixed with suitable options and (for Coq<8.5), if
`coq-load-path-include-current' is enabled, the directory base of
FILE.  The resulting list is fresh for every call, callers can
append more arguments with `nconc'.

CURRENT-DIRECTORY should be an absolute directory name.  It can be nil if
`coq-load-path-include-current' is nil.

A non-nil PRE-V85 flag requests compatibility handling of flags."
  (unless (coq-load-path-safep loadpath)
    (error "Invalid value in coq-load-path"))
  (when (and pre-v85 coq-load-path-include-current)
    (cl-assert current-directory)
    (push current-directory loadpath))
  (cl-loop for entry in loadpath
           append (coq-option-of-load-path-entry entry pre-v85)))

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

(defcustom coq-use-project-file t
  "If t, when opening a Coq file read the dominating _CoqProject.
If t, when a Coq file is opened, Proof General will look for a
project file (see `coq-project-filename') somewhere in the
current directory or its parent directories.  If there is one,
its contents are read and used to determine the arguments that
must be given to coqtop.  In particular it sets the load
path (including the -R lib options) (see `coq-load-path')."
  :type 'boolean
  :safe 'booleanp
  :group 'coq)

;; FIXME: should we add "Make" as a valid project name? Maybe only if
;; _xxProject is not present? And/Or if its content seems ok?
;; \\` and \\' avoid matching "_CoqProject~" and such
(defcustom coq-project-file-regexp "\\`\\(_coqproject\\|_rocqproject\\)\\'"
  "The regexp matching file names which PG detects as Coq/Rocq project files.

It is used by `coq--default-project-find-file' in a case insensitive way."
  :type 'string
  :safe 'stringp
  :group 'coq)


(defun coq--default-project-find-file (dir)
  "Default function for `coq-project-filename'.

If DIR contains an acceptable Coq/Rocq project file, return it.  Return
nil otherwise.  See `coq-project-filename'."
  (when (file-directory-p dir)
    (let* ((case-fold-search t) (files (directory-files dir)))
      (cl-find-if (lambda (s) (string-match coq-project-file-regexp s nil)) files))))

(defcustom coq-project-filename 'coq--default-project-find-file
  "Variable containing the function detecting a project file.

See its default value `coq--default-project-find-file' for an example.

The function takes one argument, the name of a directory, and returns
the name of a Coq/Rocq project file it contains if there is one.  Return
nil otherwise.

This is used to detect the Coq project file (using
`locate-dominating-file').  By default we accept _CoqProject and
_RocqProject (and any case-variant of these) without checking Coq/Rocq
version.  If you want something smarter please redefine
`coq-project-filename' directly by using:

\(setq coq-project-filename #'my-own-predicate)

About the Coq/Rocq project file (cf. Coq documentation on project files
and \"makefile generation\"):

The Coq project file of a Coq development should contain the arguments
given to Coq_makefile. In particular it contains the -I and -R
options (preferably one per line). If `coq-use-coqproject' is
t (default) the content of this file will be used by Proof General to
infer the `coq-load-path' and the `coq-prog-args' variables that set the
coqtop invocation by Proof General. This is now the recommended way of
configuring the coqtop invocation. Local file variables may still be
used to override the Coq project file's configuration. .dir-locals.el
files also work and override project file settings."
  :type 'function
  :group 'coq)

(defun coq-find-project-file ()
  "Return '(buf alreadyopen) where buf is the buffer visiting Coq project file.
alreadyopen is t if buffer already existed."
  (when buffer-file-name
    (let* ((projectfiledir
            (locate-dominating-file
             buffer-file-name
             (lambda (f) (funcall coq-project-filename f)))))
      (when projectfiledir
	(let* ((projectfilel (funcall coq-project-filename projectfiledir))
               (projectfile (expand-file-name projectfilel projectfiledir))
	       ;; we store this intermediate result to know if we have to kill
	       ;; the coq project buffer at the end
	       (projectbufferalreadyopen (find-buffer-visiting projectfile))
	       (projectbuffer (or projectbufferalreadyopen
				  (find-file-noselect projectfile t t))))
	  (list projectbuffer projectbufferalreadyopen))))))

(defconst coq--project-file-separator "[\r\t\n[:space:]]+")

(defconst coq--makefile-switch-arities
  '(("-R" . 2)
    ("-Q" . 2)
    ("-I" . 1)
    ("-arg" . 1)
    ("-opt" . 0)
    ("-byte" . 0)))

(defun coq--read-one-option-from-project-file (switch arity raw-args)
  "Cons SWITCH with ARITY arguments from RAW-ARGS.
If ARITY is nil, return SWITCH."
  (cond
   ((not arity) switch)
   ((< (length raw-args) arity)
    (message "Invalid _CoqProject: not enough arguments for %S" switch)
    switch)
   (t
    (let ((arguments (cl-subseq raw-args 0 arity)))
      (cons switch arguments)))))

(defun coq--read-options-from-project-file (contents)
  "Read options from CONTENTS of _CoqProject.
Returns a mixed list of option-value pairs and strings."
  (let ((raw-args (split-string-and-unquote contents coq--project-file-separator))
        (options nil))
    (while raw-args
      (let* ((switch (pop raw-args))
             (arity (cdr (assoc switch coq--makefile-switch-arities))))
        (push (coq--read-one-option-from-project-file switch arity raw-args) options)
        (setq raw-args (nthcdr (or arity 0) raw-args))))
    (nreverse options))) ; Order of options is important sometimes (Cf. #7980)

(defun coq--extract-prog-args (options)
  "Extract coqtop arguments from _CoqProject options OPTIONS.
OPTIONS is a list or conses (switch . argument) and strings.
Note that the semantics of the -arg flags in Coq project files
are weird: -arg \"a b\" means pass a and b, separately, to
coqtop.  But -arg \"'a b'\" means to pass a and b together."
  (let ((args nil))
    (dolist (opt options)
      (pcase opt
        ((or "-byte" "-op")
         (push opt args))
        (`("-arg" ,concatenated-args)
	 (if (and (string-prefix-p "'" concatenated-args) (string-suffix-p "'" concatenated-args))
	     (setq args (append args (list (substring concatenated-args 1 -1))))
           (setq args
		 (append args
			 (split-string-and-unquote concatenated-args coq--project-file-separator)))))))
    args))

(defun coq--extract-load-path-1 (option base-directory)
  "Convert one _CoqProject OPTION, relative to BASE-DIRECTORY."
  (pcase option
    (`("-I" ,path)
     (list 'ocamlimport (expand-file-name path base-directory)))
    (`("-R" ,path ,alias)
     (list 'rec (expand-file-name path base-directory) alias))
    (`("-Q" ,path ,alias)
     (list 'recnoimport (expand-file-name path base-directory) alias))))

(defun coq--extract-load-path (options base-directory)
  "Extract LoadPath from _CoqProject options OPTIONS.
OPTIONS is a list or conses (switch . arguments) and strings.
Paths are taken relative to BASE-DIRECTORY."
  (cl-loop for opt in options
           for extracted = (coq--extract-load-path-1 opt base-directory)
           when extracted collect extracted))

;; optional args allow to implement the precedence of dir/file local vars
(defun coq-load-project-file-with-avoid (&optional avoidargs avoidpath)
  "Set `coq-prog-args' and `coq-load-path' from _CoqProject.
If AVOIDARGS or AVOIDPATH is set, do not set the corresponding
variable."
  (pcase-let* ((`(,proj-file-buf ,no-kill) (coq-find-project-file)))
    (if (not proj-file-buf)
        (message "Coq project file not detected.")
      (let* ((contents (with-current-buffer proj-file-buf (buffer-string)))
             (options (coq--read-options-from-project-file contents))
             (proj-file-name (buffer-file-name proj-file-buf))
             (proj-file-dir (file-name-directory proj-file-name))
             (proj-file-local-dir (or (file-remote-p proj-file-dir 'localname)
                                      proj-file-dir)))
        (unless avoidargs (setq coq-prog-args (coq--extract-prog-args options)))
        (unless avoidpath (setq coq-load-path (coq--extract-load-path options proj-file-local-dir)))
        (let ((msg
               (cond
                ((and avoidpath avoidargs) "Coqtop args and load path")
                (avoidpath "Coqtop load path")
                (avoidargs "Coqtop args")
                (t ""))))
          (message
           "Coq project file detected: %s%s." proj-file-name
           (if (or avoidpath avoidargs)
               (concat "\n(" msg " overridden by dir/file local values)")
             "")))
        (when coq-debug
          (message "coq-prog-args: %S" coq-prog-args)
          (message "coq-load-path: %S" coq-load-path))
        (unless no-kill (kill-buffer proj-file-buf))))))

(defun coq-load-project-file ()
  "Set `coq-prog-args' and `coq-load-path' according to _CoqProject file.
Obeys `coq-use-project-file'.  Note that if a variable is already set
by dir/file local variables, this function will not override its value.
See `coq-project-filename' to change the name of the project file, and
`coq-use-project-file' to disable this feature."
  (when coq-use-project-file
    ;; Let us reread dir/file local vars, in case the user mmodified them
    (let* ((oldargs (assoc 'coq-prog-args file-local-variables-alist))
           (oldpath (assoc 'coq-load-path file-local-variables-alist)))
      (coq-load-project-file-with-avoid oldargs oldpath))))


(defun coq-load-project-file-rehack ()
  "Reread file/dir local vars and call `coq-load-project-file'.
Does nothing if `coq-use-project-file' is nil."
  (when coq-use-project-file
    ;; Let us reread dir/file local vars, in case the user mmodified them
    (hack-local-variables)
    ;; Useless since coq-load-project-file is in hack-local-variables-hook:
    ;;(coq-load-project-file)
    ))


;; Since coq-project-filename can be set via .dir-locals.el or file variable,
;; we need to call coq-load-coq-project-file only *after* local variables are
;; set. But coq-mode-hook is called BEFORE local variables are read. Therefore
;; coq-load-coq-project-file is added to hack-local-variables-hook instead. To
;; avoid adding for other modes , the setting is performed inside
;; coq-mode-hook. This is described in www.emacswiki.org/emacs/LocalVariables

;; NOTE: the coq-prog-arg variable is local to the buffer, and it is
;; re-populated each time coq is launched on this buffer. In other
;; words: this hook is useful for coq files on which coq will not be
;; run.

;; TODO: also read COQBIN somewhere?
;; Note: this does not need to be at a particular place in the hook, but we
;; need to make this hook local.
;; hack-local-variables-hook seems to hack local and dir local vars.
(add-hook 'coq-mode-hook
          (lambda ()
            (add-hook 'hack-local-variables-hook
                      #'coq-load-project-file
                      nil t)))

;; Detecting coqtop or rocq args should happen at the last moment
;; before calling the process. In particular it should happen after
;; that proof-prog-name-ask is performed. This hook is exactly called
;; after querying proof-prog-name (if proof-prog-name-ask is t). This
;; way the options can be elaborated correctly (coqtop and rocq have
;; different options). We first detect executables (taking
;; proof-prog-name-ask into account), then elaborate the
;; proof-prog-args. Note: we don't support proof-prog-name-guess
(add-hook 'proof-shell-before-process-hook
          (lambda ()
            ;; detect executables unless explicitely set by hand. In
            ;; this case we try to find coqdep and coqc in the same
            ;; directory
            (if (and proof-prog-name-ask proof-prog-name)
                ;; let us find other executables in the exact same
                ;; place. TODO: go back to default exec-path if not found?
                (let ((exec-path (list (file-name-directory proof-prog-name))))
                  (setq coq-compiler (executable-find (coq-detect-coqc)))
                  (setq coq-dependency-analyzer (executable-find (coq-detect-coqdep))))
              ;; default: detect everything from the current exec-path
              (setq proof-prog-name (coq-autodetect-progname))
              (setq coq-compiler (coq-detect-coqc))
              (setq coq-dependency-analyzer (coq-detect-coqdep)))
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



(defun coq-toggle-use-project-file ()
  (interactive)
  (setq coq-use-project-file (not coq-use-project-file))
  (when coq-use-project-file (coq-load-project-file-rehack))
  ;; FIXME What should we do when disabling project file? since
  ;; local variables override project file anyway, reading them
  ;; again is useless. Let us do nothing.
  ;;(setq coq-load-path nil)
  ;;(setq coq-prog-args nil)
  )


(provide 'coq-system)

;;; coq-system.el ends here
