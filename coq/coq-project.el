;;; coq-project.el --- Coq load path and project file  -*- lexical-binding:t -*-

;; This file is part of Proof General.

;; Portions © Copyright 1994-2012  David Aspinall and University of Edinburgh
;; Portions © Copyright 2003-2021  Free Software Foundation, Inc.
;; Portions © Copyright 2001-2026  Pierre Courtieu
;; Portions © Copyright 2010, 2016  Erik Martin-Dorel
;; Portions © Copyright 2011-2013, 2016-2017  Hendrik Tews
;; Portions © Copyright 2015-2017  Clément Pit-Claudel

;; Authors: Hendrik Tews, Pierre Courtieu
;; Maintainer: Pierre.Courtieu<Pierre.Courtieu@cnam.fr>

;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:
;;
;; The Coq load path (`coq-load-path') and the Coq project file
;; (_CoqProject, _RocqProject) setting it, with `coq-prog-args'.  It
;; needs no proof assistant.

;;; Code:

(require 'cl-lib)

(defvar coq-prog-args)
(defvar coq-debug)

;; Arbitrary arguments can already be passed through _CoqProject.
;; However this is not true for all assistants, so we don't modify the
;; (defpgcustom prog-args) declaration.
(defun coq--string-list-p (obj)
  "Determine if OBJ is a list of strings."
  (or (null obj) (and (consp obj) (stringp (car obj)) (coq--string-list-p (cdr obj)))))

(put 'coq-prog-args 'safe-local-variable #'coq--string-list-p)

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
               (eq (car entry) 'package)
               (cl-every 'stringp (cdr entry))
               (equal (length entry) 2))
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
  - A list of the form `(\\='package name)', specifying (since 9.3) a
    ocamlfind package with Rocq files.
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
                               "ocamlfind package for Rocq"
                               (const package)
                               (string :tag "name"))
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
      (`(package ,name)
       (list "-package" name))
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

\(setq coq-project-filename #\\='my-own-predicate)

About the Coq/Rocq project file (cf. Coq documentation on project files
and \"makefile generation\"):

The Coq project file of a Coq development should contain the arguments
given to Coq_makefile. In particular it contains the -I and -R
options (preferably one per line). If `coq-use-coqproject' is
t (default) the content of this file will be used by Proof General to
infer the `coq-load-path' and the `coq-prog-args' variables that set the
coqtop invocation by Proof General. This is now the recommended way of
configuring the coqtop invocation. Local file variables may still be
used to override the Coq project file\\='s configuration. .dir-locals.el
files also work and override project file settings."
  :type 'function
  :group 'coq)

(defun coq-find-project-file ()
  "Return (BUF ALREADYOPEN) where BUF is the buffer visiting Coq project file.
ALREADYOPEN is t if buffer already existed."
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

;; The syntax of project files is that of `rocq makefile'; see "The grammar of
;; _RocqProject" in the Rocq reference manual and lib/coqProject_file.ml.

(defconst coq--project-file-token-regexp
  "[ \t\n\r]+\\|#.*$\\|\"\\(?1:[^\"]*\\)\"\\|\\(?1:[^ \t\n\r#\"][^ \t\n\r#]*\\)\\|\\(?2:\"\\)"
  "Regexp matching one blank, comment or token of a project file.
Group 1 holds the token, group 2 an unterminated string.")

(defconst coq--arg-value-token-regexp "\\(?:[^ ']+\\|'[^']*'?\\)+"
  "Regexp matching one argument, quotes included, in a -arg value.")

(defun coq--project-file-tokenize (contents)
  "Split CONTENTS of a project file into tokens.
A double quote at the start of a token protects blanks and \"#\" up to
the next double quote; there is no escape character."
  (let ((pos 0) (tokens nil))
    (while (string-match coq--project-file-token-regexp contents pos)
      (setq pos (match-end 0))
      (when (match-beginning 2) (error "Unterminated string in project file"))
      (when (match-beginning 1) (push (match-string 1 contents) tokens)))
    (nreverse tokens)))

(defun coq--split-arg-value (value)
  "Split VALUE, the argument of a -arg option, into coqtop arguments."
  (when (cl-oddp (cl-count ?' value))
    (error "Unpaired single quote in -arg value: %S" value))
  (let ((pos 0) (args nil))
    (while (string-match coq--arg-value-token-regexp value pos)
      (setq pos (match-end 0))
      (push (replace-regexp-in-string "'" "" (match-string 0 value) t t) args))
    (nreverse args)))

(defconst coq--makefile-switch-arities
  '(("-R" . 2)
    ("-Q" . 2)
    ("-I" . 1)
    ("-package" . 1)
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
  (let ((raw-args (coq--project-file-tokenize contents))
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
coqtop.  But -arg \"\\='a b\\='\" means to pass a and b together."
  (let ((args nil))
    (dolist (opt options)
      (pcase opt
        ((or "-byte" "-op")
         (push opt args))
        (`("-arg" ,concatenated-args)
         (setq args (append args (coq--split-arg-value concatenated-args))))))
    args))

(defun coq--extract-load-path-1 (option base-directory)
  "Convert one _CoqProject OPTION, relative to BASE-DIRECTORY."
  (pcase option
    (`("-I" ,path)
     (list 'ocamlimport (expand-file-name path base-directory)))
    (`("-package" ,name)
     (list 'package name))
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
        (when (bound-and-true-p coq-debug)
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


;; Since coq-project-filename can be set via .dir-locals.el or file
;; variable, `coq-load-project-file' must run only *after* local
;; variables are set.  The `coq-mode' function, which runs before, adds
;; it to the buffer-local `hack-local-variables-hook'.  See
;; www.emacswiki.org/emacs/LocalVariables.

;; NOTE: the coq-prog-arg variable is local to the buffer, and it is
;; re-populated each time coq is launched on this buffer. In other
;; words: this hook is useful for coq files on which coq will not be
;; run.

;; TODO: also read COQBIN somewhere?


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

(provide 'coq-project)

;;; coq-project.el ends here
