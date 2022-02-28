;;; proof-autoloads.el --- automatically extracted autoloads

;; This file is part of Proof General.

;; Portions © Copyright 1994-2012  David Aspinall and University of Edinburgh
;; Portions © Copyright 2003-2018  Free Software Foundation, Inc.
;; Portions © Copyright 2001-2017  Pierre Courtieu
;; Portions © Copyright 2010, 2016  Erik Martin-Dorel
;; Portions © Copyright 2011-2013, 2016-2017  Hendrik Tews
;; Portions © Copyright 2015-2017  Clément Pit-Claudel

;;; Commentary:
;;

;;; Code:

(provide 'proof-autoloads)


;;;### (autoloads nil "../coq/coq" "../coq/coq.el" (0 0 0 0))
;;; Generated autoloads from ../coq/coq.el

(autoload 'coq-pg-setup "../coq/coq" nil nil nil)

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "../coq/coq" '("auto-adapt-printing-width" "build-list-id-from-string" "coq-" "count-not-intersection" "hide-additional-subgoals" "is-not-split-vertic" "last-coq-error-location" "mod" "notation-print-kinds-table" "printing-depth" "proof-" "reqkinds-kinds-table" "time-commands")))

;;;***

;;;### (autoloads nil "../coq/coq-abbrev" "../coq/coq-abbrev.el"
;;;;;;  (0 0 0 0))
;;; Generated autoloads from ../coq/coq-abbrev.el

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "../coq/coq-abbrev" '("coq-" "holes-show-doc")))

;;;***

;;;### (autoloads nil "../coq/coq-autotest" "../coq/coq-autotest.el"
;;;;;;  (0 0 0 0))
;;; Generated autoloads from ../coq/coq-autotest.el

(autoload 'coq-autotest "../coq/coq-autotest" nil t nil)

;;;***

;;;### (autoloads nil "../coq/coq-compile-common" "../coq/coq-compile-common.el"
;;;;;;  (0 0 0 0))
;;; Generated autoloads from ../coq/coq-compile-common.el

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "../coq/coq-compile-common" '("coq-" "number-of-cpus" "time-less-or-equal")))

;;;***

;;;### (autoloads nil "../coq/coq-db" "../coq/coq-db.el" (0 0 0 0))
;;; Generated autoloads from ../coq/coq-db.el

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "../coq/coq-db" '("coq-" "filter-state-" "max-length-db")))

;;;***

;;;### (autoloads nil "../coq/coq-diffs" "../coq/coq-diffs.el" (0
;;;;;;  0 0 0))
;;; Generated autoloads from ../coq/coq-diffs.el

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "../coq/coq-diffs" '("coq-")))

;;;***

;;;### (autoloads nil "../coq/coq-indent" "../coq/coq-indent.el"
;;;;;;  (0 0 0 0))
;;; Generated autoloads from ../coq/coq-indent.el

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "../coq/coq-indent" '("coq-")))

;;;***

;;;### (autoloads nil "../coq/coq-local-vars" "../coq/coq-local-vars.el"
;;;;;;  (0 0 0 0))
;;; Generated autoloads from ../coq/coq-local-vars.el

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "../coq/coq-local-vars" '("coq-")))

;;;***

;;;### (autoloads nil "../coq/coq-mode" "../coq/coq-mode.el" (0 0
;;;;;;  0 0))
;;; Generated autoloads from ../coq/coq-mode.el

(add-to-list 'auto-mode-alist '("\\.v\\'" . coq-mode))

(autoload 'coq-mode "../coq/coq-mode" "\
Major mode for Coq scripts.

\\{coq-mode-map}

\(fn)" t nil)

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "../coq/coq-mode" '("coq-" "get-coq-library-directory")))

;;;***

;;;### (autoloads nil "../coq/coq-par-compile" "../coq/coq-par-compile.el"
;;;;;;  (0 0 0 0))
;;; Generated autoloads from ../coq/coq-par-compile.el

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "../coq/coq-par-compile" '("coq-" "split-list-at-predicate")))

;;;***

;;;### (autoloads nil "../coq/coq-seq-compile" "../coq/coq-seq-compile.el"
;;;;;;  (0 0 0 0))
;;; Generated autoloads from ../coq/coq-seq-compile.el

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "../coq/coq-seq-compile" '("coq-seq-")))

;;;***

;;;### (autoloads nil "../coq/coq-smie" "../coq/coq-smie.el" (0 0
;;;;;;  0 0))
;;; Generated autoloads from ../coq/coq-smie.el

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "../coq/coq-smie" '("coq-")))

;;;***

;;;### (autoloads nil "../coq/coq-syntax" "../coq/coq-syntax.el"
;;;;;;  (0 0 0 0))
;;; Generated autoloads from ../coq/coq-syntax.el

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "../coq/coq-syntax" '("coq-" "develock-coq-font-lock-keywords")))

;;;***

;;;### (autoloads nil "../coq/coq-system" "../coq/coq-system.el"
;;;;;;  (0 0 0 0))
;;; Generated autoloads from ../coq/coq-system.el

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "../coq/coq-system" '("coq-")))

;;;***

;;;### (autoloads nil "../coq/coq-unicode-tokens" "../coq/coq-unicode-tokens.el"
;;;;;;  (0 0 0 0))
;;; Generated autoloads from ../coq/coq-unicode-tokens.el

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "../coq/coq-unicode-tokens" '("coq-")))

;;;***

;;;### (autoloads nil "../lib/bufhist" "../lib/bufhist.el" (0 0 0
;;;;;;  0))
;;; Generated autoloads from ../lib/bufhist.el

(autoload 'bufhist-mode "../lib/bufhist" "\
Minor mode retaining an in-memory history of the buffer contents.

If called interactively, enable Bufhist mode if ARG is positive,
and disable it if ARG is zero or negative.  If called from Lisp,
also enable the mode if ARG is omitted or nil, and toggle it if
ARG is `toggle'; disable the mode otherwise.

Commands:\\<bufhist-mode-map>
\\[bufhist-prev]    bufhist-prev    go back in history
\\[bufhist-next]    bufhist-next    go forward in history
\\[bufhist-first]   bufhist-first   go to first item in history
\\[bufhist-last]    bufhist-last    go to last (current) item in history.
\\[bufhist-clear]   bufhist-clear   clear history.
\\[bufhist-delete]  bufhist-clear   delete current item from history.

\(fn &optional ARG)" t nil)

(autoload 'bufhist-init "../lib/bufhist" "\
Initialise a ring history for the current buffer.
The history will be read-only unless READWRITE is non-nil.
For read-only histories, edits to the buffer switch to the latest version.
If RINGSIZE is omitted or nil, the size defaults to ‘bufhist-ring-size’.

\(fn &optional READWRITE RINGSIZE)" t nil)

(autoload 'bufhist-exit "../lib/bufhist" "\
Stop keeping ring history for current buffer." t nil)

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "../lib/bufhist" '("bufhist-")))

;;;***

;;;### (autoloads nil "../lib/holes" "../lib/holes.el" (0 0 0 0))
;;; Generated autoloads from ../lib/holes.el

(autoload 'holes-set-make-active-hole "../lib/holes" "\
Make a new hole between START and END or at point, and make it active.

\(fn &optional START END)" t nil)

(autoload 'holes-mode "../lib/holes" "\
Toggle Holes minor mode.
With arg, turn Outline minor mode on if arg is positive, off otherwise.

The mode `holes-mode' is meant to help program editing.  It is
useful to build complicated expressions by copy pasting several
peices of text from different parts of a buffer (or even from
different buffers).

HOLES

A hole is a piece of (highlighted) text that may be replaced by
another part of text later.  There is no information stored on the
file for holes, so you can save and modify files containing holes with
no harm... You can even insert or delete characters inside holes like
any other characters.

USE

At any time only one particular hole, called \"active\", can be
\"filled\".  Holes can be in several buffers but there is always one or
zero active hole globally.  It is highlighted with a different color.

Functions described below have default shortcuts when `holes-mode' is
on that you can customize.

TO DEFINE A HOLE, two methods:

 o Select a region with keyboard or mouse, then use
   \\[holes-set-make-active-hole].  If the selected region is empty,
   then a hole containing # is created at point.

 o Select text with mouse while pressing ctrl and meta (`C-M-select').
   If the selected region is empty (i.e. if you just click while
   pressing ctrl+meta), then a hole containing # is created.

TO ACTIVATE A HOLE, click on it with the button 1 of your mouse.  The
previous active hole will be deactivated.

TO FORGET A HOLE without deleting its text, click on it with the
button 2 (middle) of your mouse.

TO DESTROY A HOLE and delete its text, click on it with the button 3
of your mouse.

TO FILL A HOLE with a text selection, first make sure it is active,
then two methods:

 o Select text with keyboard or mouse and hit
   \\[holes-replace-update-active-hole]

 o Select text with mouse while pressing ctrl, meta and shift
   (`C-M-S-select').  This is a
   generalization of the `mouse-track-insert' feature of XEmacs.  This
   method allows you to fill different holes faster than with the usual
   copy-paste method.

After replacement the next hole is automatically made active so you
can fill it immediately by hitting again
\\[holes-replace-update-active-hole] or `C-M-S-select'.

TO JUMP TO THE ACTIVE HOLE, just hit
\\[holes-set-point-next-hole-destroy].  You must
be in the buffer containing the active hole.  the point will move to
the active hole, and the active hole will be destroyed so you can type
something to put at its place.  The following hole is automatically
made active, so you can hit \\[holes-set-point-next-hole-destroy]
again.

It is useful in combination with abbreviations.  For example in
`coq-mode' \"fix\" is an abbreviation for Fixpoint # (# : #) {struct #} :
# := #, where each # is a hole. Then hitting
\\[holes-set-point-next-hole-destroy] goes from one hole to the
following and you can fill-in each hole very quickly.

COMBINING HOLES AND SKELETONS

`holes' minor mode is made to work with minor mode `skeleton' minor
mode.

KNOWN BUGS

 o Don't try to make overlapping holes, it doesn't work. (what would
it mean anyway?)

 o Cutting or pasting a hole will not produce new holes, and
undoing on holes cannot make holes re-appear.

\(fn &optional ARG)" t nil)

(autoload 'holes-abbrev-complete "../lib/holes" "\
Complete abbrev by putting holes and indenting.
Moves point at beginning of expanded text.  Put this function as
call-back for your abbrevs, and just expanded \"#\" and \"@{..}\" will
become holes." nil nil)

(autoload 'holes-insert-and-expand "../lib/holes" "\
Insert S, expand it and replace #s and @{]s by holes.

\(fn S)" nil nil)

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "../lib/holes" '("hole")))

;;;***

;;;### (autoloads nil "../lib/local-vars-list" "../lib/local-vars-list.el"
;;;;;;  (0 0 0 0))
;;; Generated autoloads from ../lib/local-vars-list.el

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "../lib/local-vars-list" '("local-vars-list-")))

;;;***

;;;### (autoloads nil "../lib/maths-menu" "../lib/maths-menu.el"
;;;;;;  (0 0 0 0))
;;; Generated autoloads from ../lib/maths-menu.el

(autoload 'maths-menu-mode "../lib/maths-menu" "\
Install a menu for entering mathematical characters.
Uses window system menus only when they can display multilingual text.
Otherwise the menu-bar item activates the text-mode menu system.
This mode is only useful with a font which can display the maths repertoire.

If called interactively, enable Maths-Menu mode if ARG is
positive, and disable it if ARG is zero or negative.  If called
from Lisp, also enable the mode if ARG is omitted or nil, and
toggle it if ARG is `toggle'; disable the mode otherwise.

\(fn &optional ARG)" t nil)

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "../lib/maths-menu" '("maths-menu-")))

;;;***

;;;### (autoloads nil "../lib/pg-fontsets" "../lib/pg-fontsets.el"
;;;;;;  (0 0 0 0))
;;; Generated autoloads from ../lib/pg-fontsets.el

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "../lib/pg-fontsets" '("pg-fontsets-")))

;;;***

;;;### (autoloads nil "../lib/proof-compat" "../lib/proof-compat.el"
;;;;;;  (0 0 0 0))
;;; Generated autoloads from ../lib/proof-compat.el

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "../lib/proof-compat" '("pg-custom-undeclare-variable")))

;;;***

;;;### (autoloads nil "../lib/span" "../lib/span.el" (0 0 0 0))
;;; Generated autoloads from ../lib/span.el

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "../lib/span" '("fold-spans" "next-span" "prev-span" "set-span-" "span")))

;;;***

;;;### (autoloads nil "../qrhl/qrhl" "../qrhl/qrhl.el" (0 0 0 0))
;;; Generated autoloads from ../qrhl/qrhl.el

(let ((loads (get 'qrhl 'custom-loads))) (if (member '"../qrhl/qrhl" loads) nil (put 'qrhl 'custom-loads (cons '"../qrhl/qrhl" loads))))

(defvar qrhl-input-method "qrhl" "\
Input method to use when editing qRHL proof scripts")

(custom-autoload 'qrhl-input-method "../qrhl/qrhl" t)

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "../qrhl/qrhl" '("qrhl-")))

;;;***

;;;### (autoloads nil "pg-assoc" "pg-assoc.el" (0 0 0 0))
;;; Generated autoloads from pg-assoc.el

(autoload 'proof-associated-buffers "pg-assoc" "\
Return a list of the associated buffers.
Some may be dead/nil." nil nil)

(autoload 'proof-associated-windows "pg-assoc" "\
Return a list of the associated buffers windows.
Dead or nil buffers are not represented in the list.  Optional
argument ALL-FRAMES has the same meaning than for
`get-buffer-window'.

\(fn &optional ALL-FRAMES)" nil nil)

(autoload 'proof-associated-frames "pg-assoc" "\
Return the list of frames displaying at least one associated buffer." nil nil)

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "pg-assoc" '("proof-")))

;;;***

;;;### (autoloads nil "../lib/pg-dev" "../lib/pg-dev.el" (0 0 0 0))
;;; Generated autoloads from ../lib/pg-dev.el

(autoload 'profile-pg "../lib/pg-dev" "\
Configure Proof General for profiling.  Use \\[elp-results] to see results." t nil)

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "../lib/pg-dev" '("elp-pack-number" "pg-" "unload-pg")))

;;;***

;;;### (autoloads nil "pg-autotest" "pg-autotest.el" (0 0 0 0))
;;; Generated autoloads from pg-autotest.el

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "pg-autotest" '("pg-autotest")))

;;;***

;;;### (autoloads nil "pg-custom" "pg-custom.el" (0 0 0 0))
;;; Generated autoloads from pg-custom.el

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "pg-custom" '("completion-table" "favourites" "help-menu-entries" "keymap" "maths-menu-enable" "menu-entries" "one-command-per-line" "pro" "quit-timeout" "script-indent" "tags-program" "toolbar-entries" "unicode-tokens-enable" "use-holes")))

;;;***

;;;### (autoloads nil "pg-goals" "pg-goals.el" (0 0 0 0))
;;; Generated autoloads from pg-goals.el

(autoload 'proof-goals-config-done "pg-goals" "\
Initialise the goals buffer after the child has been configured." nil nil)

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "pg-goals" '("pg-goals-" "proof-goals-mode")))

;;;***

;;;### (autoloads nil "pg-movie" "pg-movie.el" (0 0 0 0))
;;; Generated autoloads from pg-movie.el

(autoload 'pg-movie-export "pg-movie" "\
Export the movie file from the current script buffer.
If FORCE, overwrite existing file without asking.

\(fn &optional FORCE)" t nil)

(autoload 'pg-movie-export-from "pg-movie" "\
Export the movie file that results from processing SCRIPT.

\(fn SCRIPT &optional FORCE)" t nil)

(autoload 'pg-movie-export-directory "pg-movie" "\
Export movie files from directory DIR with extension EXTN.
Existing XML files are overwritten.

\(fn DIR EXTN)" t nil)

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "pg-movie" '("pg-movie-")))

;;;***

;;;### (autoloads nil "pg-pamacs" "pg-pamacs.el" (0 0 0 0))
;;; Generated autoloads from pg-pamacs.el

(autoload 'proof-defpacustom-fn "pg-pamacs" "\
As for macro `defpacustom' but evaluating arguments.

\(fn NAME VAL ARGS)" nil nil)

(autoload 'defpacustom "pg-pamacs" "\
Define a setting NAME for the current proof assistant, default VAL.
Mainly intended for configuring settings of running provers,
which can be changed by sending commands.

In this case, NAME stands for the internal setting, flag, etc,
for the proof assistant, and a :setting and :type value should be
provided.  The :type of NAME should be one of 'integer, 'float,
'boolean, 'string.  Other types are not supported (see
`proof-menu-entry-for-setting').  They will yield an error when
constructing the proof assistant menu.

The function `proof-assistant-format' is used to format VAL.

This macro invokes the standard Emacs `defcustom' macro, so this
also defines a customizable setting inside Emacs.  The
customization variable is automatically put into the group
named after the prover.

If NAME corresponds instead to a PG internal setting, then a form :eval to
evaluate can be provided instead.

Additional properties in the ARGS prop list may include:

 pggroup   string    A grouping name for the setting, in case there are many.
		     For example, \"Timing\", \"Tracing\", etc.  Used
		     to generate sub-menus in the UI.

 pgipgcmd  string    Alternative to :setting.
		     Send a PGIP formatted command based on given string.

 pgdynamic flag      If flag is non-nil, this setting is a dynamic one
		     that is particular to the running instance of the prover.
		     Automatically set by preferences configured from PGIP
		     askprefs message.

This macro also extends the `proof-assistant-settings' list.

\(fn NAME VAL &rest ARGS)" nil t)

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "pg-pamacs" '("deflocal" "defpg" "proof-" "undefpgcustom")))

;;;***

;;;### (autoloads nil "pg-pbrpm" "pg-pbrpm.el" (0 0 0 0))
;;; Generated autoloads from pg-pbrpm.el

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "pg-pbrpm" '("pbrpm-menu-desc" "pg-pbrpm-")))

;;;***

;;;### (autoloads nil "pg-pgip" "pg-pgip.el" (0 0 0 0))
;;; Generated autoloads from pg-pgip.el

(autoload 'pg-pgip-process-packet "pg-pgip" "\
Process the command packet PGIP, which is parsed XML according to pg-xml-parse-*.
The list PGIPS may contain one or more PGIP packets, whose contents are processed.

\(fn PGIPS)" nil nil)

(autoload 'pg-pgip-maybe-askpgip "pg-pgip" "\
Send an <askpgip> message to the prover if PGIP is supported." nil nil)

(autoload 'pg-pgip-askprefs "pg-pgip" "\
Send an <askprefs> message to the prover." nil nil)

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "pg-pgip" '("pg-" "proof-assistant-idtables")))

;;;***

;;;### (autoloads nil "pg-response" "pg-response.el" (0 0 0 0))
;;; Generated autoloads from pg-response.el

(autoload 'proof-response-mode "pg-response" "\
Responses from Proof Assistant

\(fn)" t nil)

(autoload 'proof-response-config-done "pg-response" "\
Complete initialisation of a response-mode derived buffer." nil nil)

(autoload 'pg-response-maybe-erase "pg-response" "\
Erase the response buffer, according to confusing flag combinations.

Mainly, we look at `pg-response-erase-flag' and clear the
response buffer if this is non-nil, but NOT the special
symbol 'invisible.

ERASE-NEXT-TIME is the new value for the flag.

FORCE overrides the flag to force cleaning.

KEEP overrides the flag to prevent cleaning.

FORCE takes precedent over KEEP.

If CLEAN-WINDOWS is set, use `proof-clean-buffer' to do the erasing,
otherwise we use `bufhist-checkpoint-and-erase' to record an
undo history entry for the current buffer contents.

If the user option `proof-tidy-response' is nil, the buffer
will never be cleared unless FORCE is set.

No effect if there is no response buffer currently.
Returns non-nil if response buffer was cleared.

\(fn &optional ERASE-NEXT-TIME CLEAN-WINDOWS FORCE KEEP)" nil nil)

(autoload 'pg-response-display-with-face "pg-response" "\
Display STR with FACE in response buffer.

\(fn STR &optional FACE)" nil nil)

(autoload 'pg-response-message "pg-response" "\
Issue the message ARGS in the response buffer and display it.

\(fn &rest ARGS)" nil nil)

(autoload 'proof-next-error "pg-response" "\
Jump to location of next error reported in the response buffer.

A prefix arg specifies how many error messages to move;
negative means move back to previous error messages.

Optional argument ARGP means reparse the error message buffer
and start at the first error.

\(fn &optional ARGP)" t nil)

(autoload 'pg-response-has-error-location "pg-response" "\
Return non-nil if the response buffer has an error location.
See `pg-next-error-regexp'." nil nil)

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "pg-response" '("pg-" "proof-")))

;;;***

;;;### (autoloads nil "pg-user" "pg-user.el" (0 0 0 0))
;;; Generated autoloads from pg-user.el

(autoload 'proof-script-new-command-advance "pg-user" "\
Move point to a nice position for a new command, possibly inserting spaces.
Assumes that point is at the end of a command.
No effect if `proof-next-command-insert-space' is nil." t nil)

(autoload 'proof-goto-point "pg-user" "\
Assert or retract to the command at current position.
Calls `proof-assert-until-point' or `proof-retract-until-point' as
appropriate. With prefix argument RAW the omit proofs feature
\(`proof-omit-proofs-option') is temporaily disabled to check all
proofs in the asserted region.

\(fn &optional RAW)" t nil)

(autoload 'proof-process-buffer "pg-user" "\
Process the current (or script) buffer, and maybe move point to the end.
With prefix argument RAW the omit proofs feature (`proof-omit-proofs-option')
is temporaily disabled to check all proofs in the asserted region.

\(fn &optional RAW)" t nil)

(autoload 'proof-define-assistant-command "pg-user" "\
Define FN (docstring DOC): check if CMDVAR is set, then send BODY to prover.
BODY defaults to CMDVAR, a variable.

\(fn FN DOC CMDVAR &optional BODY)" nil t)

(autoload 'proof-define-assistant-command-witharg "pg-user" "\
Define FN (arg) with DOC: check CMDVAR is set, PROMPT a string and eval BODY.
The BODY can contain occurrences of arg.
CMDVAR is a variable holding a function or string.  Automatically has history.

\(fn FN DOC CMDVAR PROMPT &rest BODY)" nil t)

(autoload 'proof-electric-terminator-enable "pg-user" "\
Ensure modeline update to display new value for electric terminator.
This a function is called by the custom-set property 'proof-set-value.
It can also be used as a minor mode function: with ARG, turn on iff ARG>0

\(fn &optional ARG)" nil nil)

(autoload 'pg-slow-fontify-tracing-hint "pg-user" nil nil nil)

(autoload 'pg-response-buffers-hint "pg-user" "\


\(fn &optional NEXTBUF)" nil nil)

(autoload 'pg-jump-to-end-hint "pg-user" nil nil nil)

(autoload 'pg-processing-complete-hint "pg-user" "\
Display hint for showing end of locked region or processing complete." nil nil)

(autoload 'pg-next-error-hint "pg-user" "\
Display hint for locating error." nil nil)

(autoload 'pg-hint "pg-user" "\
Display a hint HINTMSG in the minibuffer, if `pg-show-hints' is non-nil.
The function `substitute-command-keys' is called on the argument.

\(fn HINTMSG)" nil nil)

(autoload 'pg-identifier-near-point-query "pg-user" "\
Query the prover about the identifier near point.
If the result is successful, we add a span to the buffer which has
a popup with the information in it." t nil)

(autoload 'proof-imenu-enable "pg-user" "\
Add or remove index menu." nil nil)

(autoload 'pg-previous-matching-input-from-input "pg-user" "\
Search backwards through input history for match for current input.
\(Previous history elements are earlier commands.)
With prefix argument N, search for Nth previous match.
If N is negative, search forwards for the -Nth following match.

\(fn N)" t nil)

(autoload 'pg-next-matching-input-from-input "pg-user" "\
Search forwards through input history for match for current input.
\(Following history elements are more recent commands.)
With prefix argument N, search for Nth following match.
If N is negative, search backwards for the -Nth previous match.

\(fn N)" t nil)

(autoload 'pg-add-to-input-history "pg-user" "\
Maybe add CMD to the input history.
CMD is only added to the input history if it is not a duplicate
of the last item added.

\(fn CMD)" nil nil)

(autoload 'pg-remove-from-input-history "pg-user" "\
Maybe remove CMD from the end of the input history.
This is called when the command is undone.  It's only
removed if it matches the last item in the ring.

\(fn CMD)" nil nil)

(autoload 'pg-clear-input-ring "pg-user" nil nil nil)

(autoload 'proof-autosend-enable "pg-user" "\
Enable or disable autosend behaviour.

\(fn &optional NOMSG)" nil nil)

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "pg-user" '("next-undo-elt" "pg-" "proof-")))

;;;***

;;;### (autoloads nil "pg-vars" "pg-vars.el" (0 0 0 0))
;;; Generated autoloads from pg-vars.el

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "pg-vars" '("pg-" "proof-")))

;;;***

;;;### (autoloads nil "pg-xml" "pg-xml.el" (0 0 0 0))
;;; Generated autoloads from pg-xml.el

(autoload 'pg-xml-parse-string "pg-xml" "\
Parse string in ARG, same as pg-xml-parse-buffer.

\(fn ARG)" nil nil)

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "pg-xml" '("pg-")))

;;;***

;;;### (autoloads nil "proof-auxmodes" "proof-auxmodes.el" (0 0 0
;;;;;;  0))
;;; Generated autoloads from proof-auxmodes.el

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "proof-auxmodes" '("proof-")))

;;;***

;;;### (autoloads nil "proof-config" "proof-config.el" (0 0 0 0))
;;; Generated autoloads from proof-config.el

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "proof-config" '("pbp-" "pg-" "proof-")))

;;;***

;;;### (autoloads nil "proof-depends" "proof-depends.el" (0 0 0 0))
;;; Generated autoloads from proof-depends.el

(autoload 'proof-depends-process-dependencies "proof-depends" "\
Process dependencies reported by prover, for NAME in span GSPAN.
Called from `proof-done-advancing' when a save is processed and
`proof-last-theorem-dependencies' is set.

\(fn NAME GSPAN)" nil nil)

(autoload 'proof-dependency-in-span-context-menu "proof-depends" "\
Make some menu entries showing proof dependencies of SPAN.
Use `proof-dependency-menu-system-specific' to build system
specific entries.

\(fn SPAN)" nil nil)

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "proof-depends" '("pg-" "proof-")))

;;;***

;;;### (autoloads nil "proof-easy-config" "proof-easy-config.el"
;;;;;;  (0 0 0 0))
;;; Generated autoloads from proof-easy-config.el

(autoload 'proof-easy-config "proof-easy-config" "\
Configure Proof General for a given proof assistant.
The symbol SYM and string name NAME must match those given in
  the `proof-assistant-table', which see.
Additional arguments are taken into account as a setq BODY.

\(fn SYM NAME &rest BODY)" nil t)

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "proof-easy-config" '("proof-easy-config-")))

;;;***

;;;### (autoloads nil "proof-faces" "proof-faces.el" (0 0 0 0))
;;; Generated autoloads from proof-faces.el

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "proof-faces" '("pg-defface-window-systems" "proof-")))

;;;***

;;;### (autoloads nil "proof-indent" "proof-indent.el" (0 0 0 0))
;;; Generated autoloads from proof-indent.el

(autoload 'proof-indent-line "proof-indent" "\
Indent current line of proof script, if indentation enabled." t nil)

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "proof-indent" '("proof-indent-")))

;;;***

;;;### (autoloads nil "proof-maths-menu" "proof-maths-menu.el" (0
;;;;;;  0 0 0))
;;; Generated autoloads from proof-maths-menu.el

(autoload 'proof-maths-menu-set-global "proof-maths-menu" "\
Set global status of maths-menu mode for PG buffers to be FLAG.
Turn on/off menu in all script buffers and ensure new buffers follow suit.

\(fn FLAG)" nil nil)

(autoload 'proof-maths-menu-enable "proof-maths-menu" "\
Turn on or off maths-menu mode in Proof General script buffer.
This invokes `maths-menu-mode' to toggle the setting for the current
buffer, and then sets PG's option for default to match.
Also we arrange to have maths menu mode turn itself on automatically
in future if we have just activated it for this buffer." t nil)

;;;***

;;;### (autoloads nil "proof-menu" "proof-menu.el" (0 0 0 0))
;;; Generated autoloads from proof-menu.el

(autoload 'proof-menu-define-keys "proof-menu" "\
Prover specific keymap under C-c C-a.

\(fn MAP)" nil nil)

(autoload 'proof-menu-define-main "proof-menu" nil nil nil)

(autoload 'proof-menu-define-specific "proof-menu" nil nil nil)

(autoload 'proof-aux-menu "proof-menu" "\
Construct and return PG auxiliary menu used in non-scripting buffers." nil nil)

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "proof-menu" '("proof-")))

;;;***

;;;### (autoloads nil "proof-script" "proof-script.el" (0 0 0 0))
;;; Generated autoloads from proof-script.el

(autoload 'proof-ready-for-assistant "proof-script" "\
Configure PG for symbol ASSISTANTSYM, name ASSISTANT-NAME.
If ASSISTANT-NAME is omitted, look up in `proof-assistant-table'.

\(fn ASSISTANTSYM &optional ASSISTANT-NAME)" nil nil)

(autoload 'proof-colour-locked "proof-script" "\
Alter the colour of all locked regions according to variable `proof-colour-locked'." t nil)

(autoload 'proof-unprocessed-begin "proof-script" "\
Return end of locked region in current buffer or (point-min) otherwise.
The position is actually one beyond the last locked character." nil nil)

(autoload 'proof-locked-region-full-p "proof-script" "\
Non-nil if the locked region covers all the buffer's non-whitespace.
Works on any buffer." nil nil)

(autoload 'proof-locked-region-empty-p "proof-script" "\
Non-nil if the locked region is empty.  Works on any buffer." nil nil)

(autoload 'proof-register-possibly-new-processed-file "proof-script" "\
Register a possibly new FILE as having been processed by the prover.

If INFORMPROVER is non-nil, the proof assistant will be told about this,
to co-ordinate with its internal file-management.  (Otherwise we assume
that it is a message from the proof assistant which triggers this call).
In this case, the user will be queried to save some buffers, unless
NOQUESTIONS is non-nil.

No action is taken if the file is already registered.

A warning message is issued if the register request came from the
proof assistant and Emacs has a modified buffer visiting the file.

\(fn FILE &optional INFORMPROVER NOQUESTIONS)" nil nil)

(autoload 'proof-mode "proof-script" "\
Proof General major mode class for proof scripts.
\\{proof-mode-map}

\(fn)" t nil)

(autoload 'proof-config-done "proof-script" "\
Finish setup of Proof General scripting mode.
Call this function in the derived mode for the proof assistant to
finish setup which depends on specific proof assistant configuration." nil nil)

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "proof-script" '("pg-" "proof-")))

;;;***

;;;### (autoloads nil "proof-shell" "proof-shell.el" (0 0 0 0))
;;; Generated autoloads from proof-shell.el

(autoload 'proof-shell-ready-prover "proof-shell" "\
Make sure the proof assistant is ready for a command.
If QUEUEMODE is set, succeed if the proof shell is busy but
has mode QUEUEMODE, which is a symbol or list of symbols.
Otherwise, if the shell is busy, give an error.
No change to current buffer or point.

\(fn &optional QUEUEMODE)" nil nil)

(autoload 'proof-shell-live-buffer "proof-shell" "\
Return non-nil if ‘proof-shell-buffer’ is live." nil nil)

(autoload 'proof-shell-available-p "proof-shell" "\
Return non-nil if there is a proof shell active and available.
No error messages.  Useful as menu or toolbar enabler." nil nil)

(autoload 'proof-shell-insert "proof-shell" "\
Insert STRINGS at the end of the proof shell, call `scomint-send-input'.

STRINGS is a list of strings (which will be concatenated), or a
single string.

The ACTION argument is a symbol which is typically the name of a
callback for when each string has been processed.

This calls `proof-shell-insert-hook'.  The arguments ACTION and
SCRIPTSPAN may be examined by the hook to determine how to modify
the string variable (exploiting dynamic scoping) which will be
the command actually sent to the shell.

Note that the hook is not called for the empty (null) string
or a carriage return.

We strip the string of carriage returns before inserting it
and updating `proof-marker' to point to the end of the newly
inserted text.

Do not use this function directly, or output will be lost.  It is only
used in `proof-add-to-queue' when we start processing a queue, and in
`proof-shell-exec-loop', to process the next item.

\(fn STRINGS ACTION &optional SCRIPTSPAN)" nil nil)

(autoload 'proof-start-queue "proof-shell" "\
Begin processing a queue of commands.
If START is non-nil, START and END are buffer positions in the
active scripting buffer for the queue region.

This function calls ‘proof-add-to-queue’ with args QUEUEITEMS and QUEUEMODE.

\(fn START END QUEUEITEMS &optional QUEUEMODE)" nil nil)

(autoload 'proof-extend-queue "proof-shell" "\
Extend the current queue with QUEUEITEMS, queue end END.
To make sense, the commands should correspond to processing actions
for processing a region from (buffer-queue-or-locked-end) to END.
The queue mode is set to 'advancing

\(fn END QUEUEITEMS)" nil nil)

(autoload 'proof-shell-wait "proof-shell" "\
Busy wait for `proof-shell-busy' to become nil, reading from prover.

Needed between sequences of commands to maintain synchronization,
because Proof General does not allow for the action list to be extended
in some cases.   Also is considerably faster than leaving the Emacs
top-level command loop to read from the prover.

Called by `proof-shell-invisible-command' and `proof-process-buffer'
when setting `proof-fast-process-buffer' is enabled.

If INTERRUPT-ON-INPUT is non-nil, return if input is received.

If TIMEOUTSECS is a number, time out after that many seconds.

\(fn &optional INTERRUPT-ON-INPUT TIMEOUTSECS)" nil nil)

(autoload 'proof-shell-invisible-command "proof-shell" "\
Send CMD to the proof process.
The CMD is `invisible' in the sense that it is not recorded in buffer.
CMD may be a string or a string-yielding expression.

Automatically add `proof-terminal-string' if necessary, examining
`proof-shell-no-auto-terminate-commands'.

By default, let the command be processed asynchronously.
But if optional WAIT command is non-nil, wait for processing to finish
before and after sending the command.

In case CMD is (or yields) nil, do nothing.

INVISIBLECALLBACK will be invoked after the command has finished,
if it is set.  It should probably run the hook variables
`proof-state-change-hook'.

FLAGS are additional flags to put onto the `proof-action-list'.
The flag 'invisible is always added to FLAGS.

\(fn CMD &optional WAIT INVISIBLECALLBACK &rest FLAGS)" nil nil)

(autoload 'proof-shell-invisible-cmd-get-result "proof-shell" "\
Execute CMD and return result as a string.
This expects CMD to result in some theorem prover output.
Ordinary output (and error handling) is disabled, and the result
\(contents of `proof-shell-last-output') is returned as a string.

\(fn CMD)" nil nil)

(autoload 'proof-shell-invisible-command-invisible-result "proof-shell" "\
Execute CMD for side effect in the theorem prover, waiting before and after.
Error messages are displayed as usual.

\(fn CMD)" nil nil)

(autoload 'proof-shell-mode "proof-shell" "\
Proof General shell mode class for proof assistant processes

\(fn)" t nil)

(autoload 'proof-shell-config-done "proof-shell" "\
Initialise the specific prover after the child has been configured.
Every derived shell mode should call this function at the end of
processing." nil nil)

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "proof-shell" '("pg-" "proof-")))

;;;***

;;;### (autoloads nil "proof-site" "proof-site.el" (0 0 0 0))
;;; Generated autoloads from proof-site.el

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "proof-site" '("proof")))

;;;***

;;;### (autoloads nil "proof-splash" "proof-splash.el" (0 0 0 0))
;;; Generated autoloads from proof-splash.el

(autoload 'proof-splash-display-screen "proof-splash" "\
Save window config and display Proof General splash screen.
If TIMEOUT is non-nil, time out outside this function, definitely
by end of configuring proof mode.  Otherwise, make a key
binding to remove this buffer.

\(fn &optional TIMEOUT)" t nil)

(autoload 'proof-splash-message "proof-splash" "\
Make sure the user gets welcomed one way or another." t nil)

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "proof-splash" '("pg-about" "proof-")))

;;;***

;;;### (autoloads nil "proof-syntax" "proof-syntax.el" (0 0 0 0))
;;; Generated autoloads from proof-syntax.el

(autoload 'proof-replace-regexp-in-string "proof-syntax" "\
Like ‘replace-regexp-in-string’, but set ‘case-fold-search’ to ‘proof-case-fold-search’.

\(fn REGEXP REP STRING)" nil nil)

(autoload 'proof-format "proof-syntax" "\
Format a string by matching regexps in ALIST against STRING.
ALIST contains (REGEXP . REPLACEMENT) pairs where REPLACEMENT
may be a string or sexp evaluated to get a string.

\(fn ALIST STRING)" nil nil)

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "proof-syntax" '("proof-")))

;;;***

;;;### (autoloads nil "proof-toolbar" "proof-toolbar.el" (0 0 0 0))
;;; Generated autoloads from proof-toolbar.el

(autoload 'proof-toolbar-setup "proof-toolbar" "\
Initialize Proof General toolbar and enable it for all PG buffers.
If `proof-toolbar-enable' is nil, change the buffer toolbars
back the default toolbar." t nil)
 (autoload 'proof-toolbar-toggle "proof-toolbar")

(autoload 'proof-toolbar-scripting-menu "proof-toolbar" "\
Menu made from the Proof General toolbar commands." nil nil)

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "proof-toolbar" '("proof-toolbar-")))

;;;***

;;;### (autoloads nil "proof-tree" "proof-tree.el" (0 0 0 0))
;;; Generated autoloads from proof-tree.el

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "proof-tree" '("proof-tree-")))

;;;***

;;;### (autoloads nil "proof-unicode-tokens" "proof-unicode-tokens.el"
;;;;;;  (0 0 0 0))
;;; Generated autoloads from proof-unicode-tokens.el

(autoload 'proof-unicode-tokens-mode-if-enabled "proof-unicode-tokens" "\
Turn on or off the Unicode Tokens minor mode in this buffer." nil nil)

(autoload 'proof-unicode-tokens-set-global "proof-unicode-tokens" "\
Set global status of unicode tokens mode for PG buffers to be FLAG.
Turn on/off menu in all script buffers and ensure new buffers follow suit.

\(fn FLAG)" nil nil)

(autoload 'proof-unicode-tokens-enable "proof-unicode-tokens" "\
Turn on or off Unicode tokens mode in Proof General script buffer.
This invokes `unicode-tokens-mode' to toggle the setting for the current
buffer, and then sets PG's option for default to match.
Also we arrange to have unicode tokens mode turn itself on automatically
in future if we have just activated it for this buffer.
Note: this function is called when the customize setting for the prover
is changed." t nil)

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "proof-unicode-tokens" '("proof-unicode-tokens-")))

;;;***

;;;### (autoloads nil "proof-useropts" "proof-useropts.el" (0 0 0
;;;;;;  0))
;;; Generated autoloads from proof-useropts.el

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "proof-useropts" '("pg-" "proof-")))

;;;***

;;;### (autoloads nil "proof-utils" "proof-utils.el" (0 0 0 0))
;;; Generated autoloads from proof-utils.el

(autoload 'proof-upgrade-elpa-packages "proof-utils" "\
Upgrade all ELPA packages (using package.el)." t nil)

(autoload 'proof-debug "proof-utils" "\
Issue the debugging message (format MSG ARGS) in the *PG Debug* buffer.
If flag `proof-general-debug' is nil, do nothing.

\(fn MSG &rest ARGS)" nil nil)

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "proof-utils" '("pg-" "proof-")))

;;;***

;;;### (autoloads nil "../lib/scomint" "../lib/scomint.el" (0 0 0
;;;;;;  0))
;;; Generated autoloads from ../lib/scomint.el

(autoload 'scomint-make-in-buffer "../lib/scomint" "\
Make a Comint process NAME in BUFFER, running PROGRAM.
If BUFFER is nil, it defaults to NAME surrounded by `*'s.
PROGRAM should be either a string denoting an executable program to create
via `start-file-process', or a cons pair of the form (HOST . SERVICE) denoting
a TCP connection to be opened via `open-network-stream'.  If there is already
a running process in that buffer, it is not restarted.  Optional fourth arg
STARTFILE is the name of a file to send the contents of to the process.

If PROGRAM is a string, the remaining SWITCHES are arguments to PROGRAM.

\(fn NAME BUFFER PROGRAM &optional STARTFILE &rest SWITCHES)" nil nil)

(autoload 'scomint-make "../lib/scomint" "\
Make a Comint process NAME in a buffer, running PROGRAM.
The name of the buffer is made by surrounding NAME with `*'s.
PROGRAM should be either a string denoting an executable program to create
via `start-file-process', or a cons pair of the form (HOST . SERVICE) denoting
a TCP connection to be opened via `open-network-stream'.  If there is already
a running process in that buffer, it is not restarted.  Optional third arg
STARTFILE is the name of a file to send the contents of the process to.

If PROGRAM is a string, the remaining SWITCHES are arguments to PROGRAM.

\(fn NAME PROGRAM &optional STARTFILE &rest SWITCHES)" nil nil)

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "../lib/scomint" '("scomint-")))

;;;***

;;;### (autoloads nil "../lib/texi-docstring-magic" "../lib/texi-docstring-magic.el"
;;;;;;  (0 0 0 0))
;;; Generated autoloads from ../lib/texi-docstring-magic.el

(autoload 'texi-docstring-magic "../lib/texi-docstring-magic" "\
Update all texi docstring magic annotations in buffer.
With prefix arg, no errors on unknown symbols.  (This results in
@def .. @end being deleted if not known).

\(fn &optional NOERROR)" t nil)

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "../lib/texi-docstring-magic" '("texi-docstring-")))

;;;***

;;;### (autoloads nil "../lib/unicode-chars" "../lib/unicode-chars.el"
;;;;;;  (0 0 0 0))
;;; Generated autoloads from ../lib/unicode-chars.el

(autoload 'unicode-chars-list-chars "../lib/unicode-chars" "\
Insert each Unicode character into a buffer.
Lets you see which characters are available for literal display
in your Emacs font." t nil)

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "../lib/unicode-chars" '("unicode-chars-alist")))

;;;***

;;;### (autoloads nil "../lib/unicode-tokens" "../lib/unicode-tokens.el"
;;;;;;  (0 0 0 0))
;;; Generated autoloads from ../lib/unicode-tokens.el

(autoload 'unicode-tokens-encode-str "../lib/unicode-tokens" "\
Return a unicode encoded version presentation of STR.

\(fn STR)" nil nil)

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "../lib/unicode-tokens" '("unicode-tokens-")))

;;;***

;;;### (autoloads nil nil ("../qrhl/qrhl-input.el" "proof.el") (0
;;;;;;  0 0 0))

;;;***

;; Local Variables:
;; version-control: never
;; no-update-autoloads: t
;; End:

;;; proof-autoloads.el ends here
