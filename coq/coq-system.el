;; coq-system.el --- common part of compilation feature

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

;;; Commentary:
;;
;; This file holds constants, options and some general functions for
;; setting coq command arguments. Some code is dedicated as a light
;; support for older versions of coq.
;;

(require 'proof)

(require 'coq-error)

(eval-when-compile
  (require 'cl-lib)
  (require 'proof-compat))

(eval-when-compile
  (defvar coq-prog-args)
  (defvar coq-debug))

(defcustom coq-prog-env nil
  "List of environment settings d to pass to Coq process.
On Windows you might need something like:
  (setq coq-prog-env '(\"HOME=C:\\Program Files\\Coq\\\"))"
  :group 'coq)
(require 'coq-error)

(defcustom coq-prog-name
  (if (executable-find "coqtop") "coqtop"
    (proof-locate-executable "coqtop" t '("C:/Program Files/Coq/bin")))

  "*Name of program to run as Coq. See `proof-prog-name', set from this.
On Windows with latest Coq package you might need something like:
   C:/Program Files/Coq/bin/coqtop.opt.exe
instead of just \"coqtop\".
This must be a single program name with no arguments; see `coq-prog-args'
to manually adjust the arguments to the Coq process.
See also `coq-prog-env' to adjust the environment."
  :type 'string
  :group 'coq)

(defcustom coq-dependency-analyzer
  (if (executable-find "coqdep") "coqdep"
    (proof-locate-executable "coqdep" t '("C:/Program Files/Coq/bin")))
  "Command to invoke coqdep."
  :type 'string
  :group 'coq)

(defcustom coq-compiler
  (if (executable-find "coqc") "coqc"
    (proof-locate-executable "coqc" t '("C:/Program Files/Coq/bin")))
  "Command to invoke the coq compiler."
  :type 'string
  :group 'coq)

(defun get-coq-library-directory ()
  (let ((default-directory
	  (if (file-accessible-directory-p default-directory)
	      default-directory
	    "/")))
    (or (ignore-errors (car (process-lines coq-prog-name "-where")))
	"/usr/local/lib/coq")))

(defconst coq-library-directory (get-coq-library-directory) ;; FIXME Should be refreshed more often
  "The coq library directory, as reported by \"coqtop -where\".")

(defcustom coq-allow-async-proofs (not (member system-type '(windows-nt cygwin)))
  "Whether to allow coqtop to run asynchronous proofs using worker 
processes. By default, not enabled for Windows, for stability reasons."
  :type 'boolean
  :group 'coq)

(defcustom coq-use-header-line t
  "Use mouse-clickable header line"
  :type 'boolean
  :group 'coq)

(defcustom coq-async-lazy nil
  "Use lazy mode for Coq asynchronous processing"
  :type 'boolean
  :group 'coq)

(defcustom coq-num-async-workers nil
  "Number of worker processes in async mode, nil for default"
  :type 'integer
  :group 'coq)

(defcustom coq-num-async-par-workers nil
  "Number of worker processes when using Ltac :par, nil for default"
  :type 'integer
  :group 'coq)

(defcustom coq-tags (concat coq-library-directory "/theories/TAGS")
  "The default TAGS table for the Coq library."
  :type 'string
  :group 'coq)

(defcustom coq-pinned-version nil 
  "Which version of Coq you are using.
There should be no need to set this value unless you use the trunk from
the Coq github repository. For Coq versions with decent version numbers
Proof General detects the version automatically and adjusts itself. This
variable should contain nil or a version string."
  :type 'string
  :group 'coq)

(defvar coq-autodetected-version nil
  "Version of Coq, as autodetected by `coq-autodetect-version'.")

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

(defun coq-autodetect-version (&optional interactive-p)
  "Detect and record the version of Coq currently in use.
Interactively (with INTERACTIVE-P), show that number."
  (interactive '(t))
  (setq coq-autodetected-version nil)
  (with-temp-buffer
    ;; Use `shell-command' via `find-file-name-handler' instead of
    ;; `process-line': when the buffer is running TRAMP, PG uses
    ;; `start-file-process', loading the binary from the remote server.
    (let* ((default-directory
	     (if (file-accessible-directory-p default-directory)
		 default-directory
	       "/"))
	   (coq-command (shell-quote-argument (or coq-prog-name "coqtop")))
           (shell-command-str (format "%s -v" coq-command))
           (fh (find-file-name-handler default-directory 'shell-command))
           (retv (if fh (funcall fh 'shell-command shell-command-str (current-buffer))
                   (shell-command shell-command-str (current-buffer)))))
      (when (equal 0 retv)
        ;; Fail silently (in that case we'll just assume Coq 8.5)
        (goto-char (point-min))
        (when (re-search-forward "version \\([^ ]+\\)" nil t)
          (setq coq-autodetected-version (match-string 1))))))
  (when interactive-p
    (coq-show-version))
  coq-autodetected-version)

(defun coq--version< (v1 v2)
  "Compare Coq versions V1 and V2."
  ;; -snapshot is only supported by Emacs 24.5, not 24.3
  (let ((version-regexp-alist `(("^[-_+ ]?snapshot$" . -4)
                                ("^pl$" . 0)
				("^~alpha$" . -3)
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
    (pcase coq-version-to-use
      ;; assume trunk is at least 8.5
      ("trunk" nil) 
      (t 
       (condition-case err
	   (coq--version< coq-version-to-use "8.5snapshot")
	 (error
	  (cond
	   ((equal (substring (cadr err) 0 15) "Invalid version")
	    (signal 'coq-unclassifiable-version  coq-version-to-use))
	   (t (signal (car err) (cdr err))))))))))

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
  "Non-standard coq library load path.
This list specifies the LoadPath extension for coqdep, coqc and
coqtop. Usually, the elements of this list are strings (for
\"-I\") or lists of two strings (for \"-R\" dir path and
\"-Q\" dir path).

The possible forms of elements of this list correspond to the 4
forms of include options (`-I' `-Q' and `-R'). An element can be

  - A list of the form `(\\='ocamlimport dir)', specifying (in 8.5) a
    directory to be added to ocaml path (`-I').
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
contain the coq standard library or \".\" for the current
directory."
  :type '(repeat (choice (string :tag "simple directory without path (-Q \"\") in 8.5")
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

(defun coq-option-of-load-path-entry (entry)
  "Translate a single ENTRY from `coq-load-path' into options.
See `coq-load-path' for the possible forms of ENTRY and to which
options they are translated.  Use a non-nil PRE-V85 flag to
request compatibility handling of flags."
    (pcase entry
      ((and (pred stringp) dir)
       (list "-Q" (expand-file-name dir) ""))
      (`(ocamlimport ,dir)
       (list "-I" (expand-file-name dir)))
      (`(recnoimport ,dir ,alias)
       (list "-Q" (expand-file-name dir) alias))
      ((or `(rec ,dir ,alias) `(,dir ,alias))
       (list "-R" (expand-file-name dir) alias))))

(defun coq-include-options (load-path &optional current-directory pre-v85)
  "Build the base list of include options for coqc, coqdep and coqtop.
The options list includes all entries from argument LOAD-PATH
\(which should be `coq-load-path' of the buffer that invoked the
compilation) prefixed with suitable options. The resulting list is fresh 
for every call, callers can append more arguments with `nconc'.

CURRENT-DIRECTORY should be an absolute directory name."
  (unless (coq-load-path-safep load-path)
    (error "Invalid value in coq-load-path"))
  (cl-loop for entry in load-path
           append (coq-option-of-load-path-entry entry)))

(defun coq--options-test-roundtrip-1 (coq-project parsed)
  "Run a sanity check on COQ-PROJECT's PARSED options."
  (let* ((concatenated (apply #'append parsed))
         (extracted (coq--extract-load-path parsed nil))
         (roundtrip (coq-include-options extracted nil)))
    (princ (format "[%s]: " coq-project))
    (if (equal concatenated roundtrip)
        (princ "OK\n")
      (princ (format "Failed.\n:: Original:  %S\n:: LoadPath: %S\n:: Roundtrip: %S\n"
                     concatenated extracted roundtrip)))))

(defun coq--options-test-roundtrip (coq-project)
  "Run a sanity check on COQ-PROJECT."
  (let ((parsed (coq--read-options-from-project-file coq-project)))
    (coq--options-test-roundtrip-1 coq-project parsed)))

(defun coq--options-test-roundtrips ()
  "Run sanity tests on coq-project parsing code.
More precisely, check that parsing and reprinting the include
options of a few coq-project files does the right thing."
  (with-output-to-temp-buffer "*tests*"
    (coq--options-test-roundtrip "-Q /test Test")
    (coq--options-test-roundtrip "-Q /test \"\"")
    (coq--options-test-roundtrip "-R /test Test")
    (coq--options-test-roundtrip "-I /test")))

(defun coq-coqdep-prog-args (load-path &optional current-directory)
  "Build a list of options for coqdep.
LOAD-PATH, CURRENT-DIRECTORY: see `coq-include-options'."
  (coq-include-options load-path current-directory))

(defun coq-coqc-prog-args (load-path &optional current-directory)
  "Build a list of options for coqc.
LOAD-PATH, CURRENT-DIRECTORY: see `coq-include-options'."
  ;; coqtop always adds the current directory to the LoadPath, so don't
  ;; include it in the -Q options.
  (append coq-prog-args
	  (coq-coqdep-prog-args coq-load-path current-directory)))

(defvar coq-coqtop-server-flags
					; TODO allow ports for main-channel
					; TODO add control-channel ports
  '("-ideslave" "-main-channel" "stdfds"))

(defvar coq-coqtop-async-flags
  (let ((proof-workers-flags
	 (and coq-num-async-workers
	      `("-async-proofs-j" ,(number-to-string coq-num-async-workers))))
	(par-workers-flags
	 (and coq-num-async-par-workers
	      `("-async-proofs-tac-j" ,(number-to-string coq-num-async-par-workers)))))
    (append '("-async-proofs" "on")
	    proof-workers-flags
	    par-workers-flags)))

(defvar coq-coqtop-no-async-flags
  '("-async-proofs" "off"))

(defvar coq-coqtop-async-lazy-flags
  '("-async-proofs" "lazy"))

;;XXXXXXXXXXXXXX
;; coq-coqtop-prog-args is the user-set list of arguments to pass to
;; Coq process, see 'defpacustom prog-args' in pg-custom.el for
;; documentation.

(defun coq-coqtop-prog-args (load-path &optional current-directory)
  ;; coqtop always adds the current directory to the LoadPath, so don't                                                                                                
  ;; include it in the -Q options. This is not true for coqdep.                                                                                                        
  "Build a list of options for coqc. 
   LOAD-PATH, CURRENT-DIRECTORY: see `coq-coqc-prog-args'."
  (let* ((coqc-args (coq-coqc-prog-args load-path current-directory))
	 (async-args (if coq-allow-async-proofs
			 coq-coqtop-async-flags
		       coq-coqtop-no-async-flags))
	 (ide-args coq-coqtop-server-flags)
	 (async-lazy-args (when (and coq-allow-async-proofs
				     coq-async-lazy)
			    coq-coqtop-async-lazy-flags)))
    (append async-args ide-args async-lazy-args coqc-args)))

(defun coq-prog-args ()
  "Recompute `coq-load-path' before calling `coq-coqtop-prog-args'."
  (coq-load-project-file)
  (coq-autodetect-version)
  (if (coq--pre-v85)
      (progn
	(message-box "Proof General requires Coq v8.5 or later.\nDetected Coq version %s" coq-autodetected-version)
	nil)
    (coq-coqtop-prog-args coq-load-path)))

(defcustom coq-use-project-file t
  "If t, when opening a coq file read the dominating _CoqProject.
If t, when a coq file is opened, Proof General will look for a
project file (see `coq-project-filename') somewhere in the
current directory or its parent directories.  If there is one,
its contents are read and used to determine the arguments that
must be given to coqtop.  In particular it sets the load
path (including the -R lib options) (see `coq-load-path')."
  :type 'boolean
  :safe 'booleanp
  :group 'coq)

(defcustom coq-project-filename "_CoqProject"
  "The name of coq project file.
The coq project file of a coq developpement (Cf Coq documentation
on \"makefile generation\") should contain the arguments given to
coq_makefile. In particular it contains the -I and -R
options (preferably one per line). If `coq-use-coqproject' is
t (default) the content of this file will be used by proofgeneral
to infer the `coq-load-path' and the `coq-prog-args' variables
that set the coqtop invocation by proofgeneral. This is now the
recommended way of configuring the coqtop invocation. Local file
variables may still be used to override the coq project file's
configuration. .dir-locals.el files also work and override
project file settings."
  :type 'string
  :safe 'stringp
  :group 'coq)

(defun coq-find-project-file ()
  "Return '(buf alreadyopen) where buf is the buffer visiting coq project file.
alreadyopen is t if buffer already existed."
  (when buffer-file-name
    (let* ((projectfiledir (locate-dominating-file buffer-file-name coq-project-filename)))
      (when projectfiledir
	(let* ((projectfile (expand-file-name coq-project-filename projectfiledir))
	       ;; we store this intermediate result to know if we have to kill
	       ;; the coq project buffer at the end
	       (projectbufferalreadyopen (find-buffer-visiting projectfile))
	       (projectbuffer (or projectbufferalreadyopen
				  (find-file-noselect projectfile t t))))
	  (list projectbuffer projectbufferalreadyopen))))))

(defconst coq--project-file-separator "[\r\n[:space:]]+")

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
  (if arity
      (let ((arguments
	     (condition-case-unless-debug nil
		 (cl-subseq raw-args 0 arity)
	       (warn "Invalid _CoqProject: not enough arguments for %S" switch))))
	(cons switch arguments))
    switch))

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
    options))

(defun coq--extract-prog-args (options)
  "Extract coqtop arguments from _CoqProject options OPTIONS.
OPTIONS is a list or conses (switch . argument) and strings.
Note that the semantics of the -arg flags in coq project files
are weird: -arg \"a b\" means pass a and b, separately, to
coqtop."
  (let ((args nil))
    (dolist (opt options)
      (pcase opt
        ((or "-byte" "-op")
         (push opt args))
        (`("-arg" ,concatenated-args)
         (setq args
               (append (split-string-and-unquote (cadr opt) coq--project-file-separator)
                       args)))))
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
  "Extract loadpath from _CoqProject options OPTIONS.
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
	     (proj-file-dir (file-name-directory proj-file-name)))
	(unless avoidargs (setq coq-prog-args (coq--extract-prog-args options)))
	(unless avoidpath (setq coq-load-path (coq--extract-load-path options proj-file-dir)))
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
Obeys `coq-use-project-file'.  Note that if a variable is already
set by dir/file local variables, this function will not override
its value.
See `coq-project-filename' to change the name of the
project file, and `coq-use-project-file' to disable this
feature."
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

;; TODO: also read COQBIN somewhere?
;; Note: this does not need to be at a particular place in the hook, but we
;; need to make this hook local.
;; hack-local-variables-hook seems to hack local and dir local vars.
(add-hook 'coq-mode-hook
	  '(lambda ()
	     (add-hook 'hack-local-variables-hook
		       'coq-load-project-file
		       nil t)))

;; smie's parenthesis blinking is too slow, let us have the default one back
(add-hook 'coq-mode-hook
	  '(lambda ()
	     (when (and (fboundp 'show-paren--default)
			(boundp 'show-paren-data-function))
	       (setq show-paren-data-function 'show-paren--default))))

(add-hook 'coq-mode-hook 'coq-header-line-init)
(add-hook 'coq-mode-hook 'coq-error-start-timer)

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



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;    To guess the command line options   ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; OBSOLETE, should take _CoqProject into account.
(defun coq-guess-command-line (filename)
  "Guess the right command line options to compile FILENAME using `make -n'.
This function is obsolete, the recommended way of setting the
coqtop options is to use a _Coqproject file as described in coq
documentation. ProofGeneral reads this file and sets compilation
options according to its contents. See `coq-project-filename'. Per file configuration may
then be set using local file variables."
  (if (local-variable-p 'coq-prog-name (current-buffer))
      coq-prog-name
    (let* ((dir (or (file-name-directory filename) "."))
	   (makedir
	    (cond
	     ((file-exists-p (concat dir "Makefile")) ".")
	     ;; ((file-exists-p (concat dir "../Makefile")) "..")
	     ;; ((file-exists-p (concat dir "../../Makefile")) "../..")
	     (t nil))))
      (if (and coq-use-makefile makedir)
	  (let*
	      ;;TODO, add dir name when makefile is in .. or ../..
	      ;;
	      ;; da: FIXME: this code causes problems if the make
	      ;; command fails.  It should not be used by default
	      ;; and should be made more robust.
	      ;;
	      ((compiled-file (concat (substring
				       filename 0
				       (string-match ".v$" filename)) ".vo"))
	       (command (shell-command-to-string
			 (concat  "cd " dir ";"
				  "make -n -W " filename " " compiled-file
				  "| sed s/coqc/coqtop/"))))
	    (message command)
	    (setq coq-prog-args nil)
	    (substring command 0 (string-match " [^ ]*$" command)))
	coq-prog-name))))

;;; coq-system.el ends here
