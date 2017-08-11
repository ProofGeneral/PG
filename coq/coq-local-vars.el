;;; coq-local-vars.el --- local variable list tools for coq

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

(require 'local-vars-list)              ; in lib directory

(eval-when-compile
  (require 'cl-lib))

(eval-when-compile
  (defvar coq-prog-name)
  (defvar coq-load-path))

;;; Code:
(defconst coq-local-vars-doc nil
  "Documentation-only variable.

PROJECT FILE

The recommended way of setting coqtop options (-I, -R and others)
is to use a project file. See the coq documentation (\"generating
makefile\") for details. The default name of the project file is
\"_CoqProject\" (can be configured via `coq-project-filename')
and its content should be a list of options to be given to
coq_makefile (one option per line). Here is an example:

-R foo bar
-I foo2
-arg -foo3
...(optionally followed by all .v files to be compiled)

If `coq-use-project-file' is t (default) ProofGeneral reads the
project file and sets coqtop options accordingly (via variables
`coq-load-path' and `coq-prog-args'). In this example the coqtop
invocation will be:

coqtop -foo3 -R foo bar -I foo2

FILE VARIABLES

If for some reason you want to avoid or override the project file
method, you can use the file variables (info:(Emacs)File
Variables). This feature of Emacs allows to set Emacs variables
on a per-file basis. File Variables are (usually) written as a
list at the end of the file.

We provide the following feature to help you:

\\[coq-ask-insert-coq-prog-name] builds a standard file variable
list for a coq file by asking you some questions. It is
accessible in the menu
`Coq' -> `COQ PROG (ARGS)' -> `Set coq prog *persistently*'.

You should be able to use this feature without reading the rest
of this documentation, which explains how it is used for coq. For
more precision, refer to the Emacs info manual at ((Emacs)File
Variables).

In Coq projects involving multiple directories, it is usually
necessary to set the variable `coq-load-path' for each file. For
example, if the file .../dir/bar/foo.v builds on material in
.../dir/theories/, then one would put the following local
variable section at the end of foo.v:

\(*
*** Local Variables:
*** coq-load-path: ((\"../util\" \"util\") \"../theories\")
*** End:
*)

This way the necessary \"-I\" options are added to all
invocations of `coqtop', `coqdep' and `coqc'. Note that the
option \"-I\" \"../theories\" is specific to file foo.v. Setting
`coq-load-path' globally via Emacs Customization is therefore
usually inappropriate. With this local variables list, Emacs will
set `coq-load-path' only when inside a buffer that visits foo.v.
Other buffers can have their own value of
`coq-load-path' (probably coming from their own local variable
lists).

If you use `make' for the compilation of coq modules you can set
`coq-compile-command' as local variable. For instance, if the
makefile is located in \".../dir\", you could set

\(*
*** Local Variables:
*** coq-load-path: (\"../theories\")
*** coq-compile-command: \"make -C .. %p/%o\"
*** End:
*)

This way, if foo.v contains the command \"Require bar.\" then
\"make -C .. .../theories/bar.vo\" will run, just before the
require command is sent to coqtop (assuming that bar.v is found
in .../theories).

\(Note that `coq-compile-command' is quite flexible because of
its use of substitution keys. A file local setting of
`coq-compile-command' should therefore usually not be
necessary.)")


(defun coq-insert-coq-prog-name (progname coqloadpath)
  "Insert or modify the local variables `coq-prog-name' and `coq-load-path'.
Set them to PROGNAME and PROGARGS respectively.  These variables describe the
coqtop command to be launched on this file."
  (add-file-local-variable 'coq-prog-name progname)
  (add-file-local-variable 'coq-load-path coqloadpath)
  ;; coq-guess-command-line uses coq-prog-name, so set it
  (make-local-variable 'coq-prog-name)
  (setq coq-prog-name progname)
  (make-local-variable 'coq-load-path)
  (setq coq-load-path coqloadpath))


(defun coq-read-directory (prompt &optional default maynotmatch initialcontent)
  "Ask for (using PROMPT) and return a directory name.
Do not insert the default directory."
  (let*
      ;; read-file-name here because it is convenient to see .v files when
      ;; selecting directories to add to the path. Moreover read-directory-name
      ;; does not seem to exist in fsf emacs?? temporarily disable graphical
      ;; dialog, as read-file-name does not allow to select a directory
      ((current-use-dialog-box use-dialog-box)
       (dummy (setq use-dialog-box nil))
       (fname (file-name-nondirectory default))
       (dir (file-name-directory default))
       (path
        (read-file-name prompt dir fname (not maynotmatch) initialcontent)))
    (setq use-dialog-box current-use-dialog-box)
    path))

(defun coq-ask-load-path (&optional olddirs)
  "Ask for and return the information to put into `coq-load-path'.
Optional argument OLDVALUE specifies the previous value of `coq-load-path', it
will be used to suggest values to the user."
  (let (loadpath option)
    ;; first suggest previous directories
    (dolist (olddir olddirs)
      (if (y-or-n-p (format "Keep the directory %s? " olddir))
          (setq loadpath (cons olddir loadpath))))
    ;; then ask for more
    (setq option (coq-read-directory
                  "Add directory (TAB to complete, empty to stop): -I " ""))
    (while (not (string-equal option ""))
      (setq loadpath (cons option loadpath)) ;reversed
      (setq option (coq-read-directory
                    "Add directory (TAB to complete, empty to stop): -I " "")))
    (message "Note: Syntax for option \"-R physicalpath logicalpath\":\ncoq-load-path: ((\"physicalpath\" \"logicalpath\") <other paths>)")
    (reverse loadpath)))

(defun coq-ask-prog-name (&optional oldvalue)
  "Ask for and return the local variables `coq-prog-name'.
These variable describes the coqtop command to be launched on this file.
Optional argument OLDVALUE specifies the previous value of `coq-prog-name', it
will be used to suggest a value to the user."
  (let* ((deflt (or oldvalue "coqtop"))
         (cmd (coq-read-directory
               (concat "coq program name (default \"" oldvalue "\"): ")
               deflt t))
         (cmd (if (string-equal cmd "") oldvalue cmd)))
    (if (and
         (string-match " " cmd)
         (not (y-or-n-p "The prog name contains spaces, are you sure ? ")))
        (coq-ask-prog-name oldvalue) ; retry
      cmd)))


(defun coq-ask-insert-coq-prog-name ()
  "Ask for and insert the local variables `coq-prog-name' and `coq-prog-args'.
These variables describe the coqtop command to be launched on this file."
  (interactive)
  (let* ((oldname (local-vars-list-get-safe 'coq-prog-name))
         (oldpath (local-vars-list-get-safe 'coq-load-path))
         (progname (coq-ask-prog-name oldname))
         (loadpath (coq-ask-load-path oldpath)))
    (coq-insert-coq-prog-name progname loadpath)))


(provide 'coq-local-vars)

;;; coq-local-vars.el ends here

;;   Local Variables: ***
;;   fill-column: 79 ***
;;   indent-tabs-mode: nil ***
;;   End: ***
