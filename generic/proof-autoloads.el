;;; proof-autoloads.el --- automatically extracted autoloads
;;
;;; Code:

(if (featurep 'proof-autoloads) (error "Already loaded"))
  
(eval-when-compile
  (require 'cl-lib))

(cl-eval-when (compile)
  (require 'pg-vars)
  (require 'proof-config))

(provide 'proof-autoloads)


;;;### (autoloads (bufhist-exit bufhist-init bufhist-mode) "../lib/bufhist"
;;;;;;  "../lib/bufhist.el" (22396 10805 387884 342000))
;;; Generated autoloads from ../lib/bufhist.el

(autoload 'bufhist-mode "../lib/bufhist" "\
Minor mode retaining an in-memory history of the buffer contents.

Commands:\\<bufhist-minor-mode-map>
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
The size defaults to `bufhist-ring-size'.

\(fn &optional READWRITE RINGSIZE)" t nil)

(autoload 'bufhist-exit "../lib/bufhist" "\
Stop keeping ring history for current buffer.

\(fn)" t nil)

;;;***

;;;### (autoloads nil "../lib/holes" "../lib/holes.el" (22432 56150
;;;;;;  44148 359000))
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
become holes.

\(fn)" nil nil)

(autoload 'holes-insert-and-expand "../lib/holes" "\
Insert S, expand it and replace #s and @{]s by holes.

\(fn S)" nil nil)

;;;***

;;;### (autoloads (maths-menu-mode) "../lib/maths-menu" "../lib/maths-menu.el"
;;;;;;  (22396 10805 387884 342000))
;;; Generated autoloads from ../lib/maths-menu.el

(autoload 'maths-menu-mode "../lib/maths-menu" "\
Install a menu for entering mathematical characters.
Uses window system menus only when they can display multilingual text.
Otherwise the menu-bar item activates the text-mode menu system.
This mode is only useful with a font which can display the maths repertoire.

\(fn &optional ARG)" t nil)

;;;***

;;;### (autoloads nil "pg-assoc" "pg-assoc.el" (22432 58393 128270
;;;;;;  113000))
;;; Generated autoloads from pg-assoc.el

(autoload 'proof-associated-buffers "pg-assoc" "\
Return a list of the associated buffers.
Some may be dead/nil.

\(fn)" nil nil)

(autoload 'proof-associated-windows "pg-assoc" "\
Return a list of the associated buffers windows.
Dead or nil buffers are not represented in the list. Optional
argument ALL-FRAMES has the same meaning than for
`get-buffer-window'.

\(fn &optional ALL-FRAMES)" nil nil)

(autoload 'proof-associated-frames "pg-assoc" "\
Return the list of frames displaying at least one associated buffer.

\(fn)" nil nil)

;;;***

;;;### (autoloads nil "../lib/pg-dev" "../lib/pg-dev.el" (22432 56187
;;;;;;  356150 385000))
;;; Generated autoloads from ../lib/pg-dev.el

(autoload 'profile-pg "../lib/pg-dev" "\
Configure Proof General for profiling.  Use M-x elp-results to see results.

\(fn)" t nil)

;;;***

;;;### (autoloads nil "pg-goals" "pg-goals.el" (22432 58268 584263
;;;;;;  353000))
;;; Generated autoloads from pg-goals.el

(autoload 'proof-goals-config-done "pg-goals" "\
Initialise the goals buffer after the child has been configured.

\(fn)" nil nil)

;;;***

;;;### (autoloads (pg-movie-export-directory pg-movie-export-from
;;;;;;  pg-movie-export) "pg-movie" "pg-movie.el" (22396 10805 375884
;;;;;;  342000))
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

;;;***

;;;### (autoloads (defpacustom proof-defpacustom-fn) "pg-pamacs"
;;;;;;  "pg-pamacs.el" (22424 64805 521004 784000))
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
'boolean, 'string.

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

 pgdynamic flag      If flag is non-nil, this setting is a dynamic one
		     that is particular to the running instance of the prover.

This macro also extends the `proof-assistant-settings' list.

\(fn NAME VAL &rest ARGS)" nil t)

;;;***

;;;### (autoloads (pg-response-has-error-location proof-next-error
;;;;;;  pg-response-message pg-response-display-with-face pg-response-maybe-erase
;;;;;;  proof-response-config-done proof-response-mode) "pg-response"
;;;;;;  "pg-response.el" (22396 10818 499884 55000))
;;; Generated autoloads from pg-response.el

(autoload 'proof-response-mode "pg-response" "\
Responses from Proof Assistant

\(fn)" t nil)

(autoload 'proof-response-config-done "pg-response" "\
Complete initialisation of a response-mode derived buffer.

\(fn)" nil nil)

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
See `pg-next-error-regexp'.

\(fn)" nil nil)

;;;***

;;;### (autoloads nil "pg-user" "pg-user.el" (22432 58106 204254
;;;;;;  539000))
;;; Generated autoloads from pg-user.el

(autoload 'proof-script-new-command-advance "pg-user" "\
Move point to a nice position for a new command, possibly inserting spaces.
Assumes that point is at the end of a command.  
No effect if `proof-next-command-insert-space' is nil.

\(fn)" t nil)

(autoload 'proof-goto-point "pg-user" "\
Assert or retract to the command at current position.
Calls `proof-assert-until-point' or `proof-retract-until-point' as
appropriate.

\(fn)" t nil)

(autoload 'proof-process-buffer "pg-user" "\
Process the current (or script) buffer, and maybe move point to the end.

\(fn)" t nil)

(autoload 'proof-define-assistant-command "pg-user" "\
Define FN (docstring DOC) to send BODY to prover, based on CMDVAR.
BODY defaults to CMDVAR, a variable.

\(fn FN DOC CMDVAR &optional BODY)" nil t)

(autoload 'proof-define-assistant-command-witharg "pg-user" "\
Define command FN to prompt for string CMDVAR to proof assistant.
CMDVAR is a variable holding a function or string.  Automatically has history.

\(fn FN DOC CMDVAR PROMPT &rest BODY)" nil t)

(autoload 'proof-electric-terminator-enable "pg-user" "\
Ensure modeline update to display new value for electric terminator.
This a function is called by the custom-set property 'proof-set-value.
It can also be used as a minor mode function: with ARG, turn on iff ARG>0

\(fn &optional ARG)" nil nil)

(autoload 'pg-slow-fontify-tracing-hint "pg-user" "\


\(fn)" nil nil)

(autoload 'pg-response-buffers-hint "pg-user" "\


\(fn &optional NEXTBUF)" nil nil)

(autoload 'pg-jump-to-end-hint "pg-user" "\


\(fn)" nil nil)

(autoload 'pg-processing-complete-hint "pg-user" "\
Display hint for showing end of locked region or processing complete.

\(fn)" nil nil)

(autoload 'pg-next-error-hint "pg-user" "\
Display hint for locating error.

\(fn)" nil nil)

(autoload 'pg-hint "pg-user" "\
Display a hint HINTMSG in the minibuffer, if `pg-show-hints' is non-nil.
The function `substitute-command-keys' is called on the argument.

\(fn HINTMSG)" nil nil)

(autoload 'pg-identifier-near-point-query "pg-user" "\
Query the prover about the identifier near point.
If the result is successful, we add a span to the buffer which has
a popup with the information in it.

\(fn)" t nil)

(autoload 'proof-imenu-enable "pg-user" "\
Add or remove index menu.

\(fn)" nil nil)

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

(autoload 'pg-clear-input-ring "pg-user" "\


\(fn)" nil nil)

(autoload 'proof-autosend-enable "pg-user" "\
Enable or disable autosend behaviour.

\(fn &optional NOMSG)" nil nil)

;;;***

;;;### (autoloads nil "pg-xml" "pg-xml.el" (22432 57780 152236 841000))
;;; Generated autoloads from pg-xml.el

(autoload 'pg-xml-parse-string "pg-xml" "\
Parse string in ARG, same as pg-xml-parse-buffer.

\(fn ARG)" nil nil)

;;;***

;;;### (autoloads nil "proof-depends" "proof-depends.el" (22432 57854
;;;;;;  740240 890000))
;;; Generated autoloads from proof-depends.el

(autoload 'proof-depends-process-dependencies "proof-depends" "\
Process dependencies reported by prover, for NAME in span GSPAN.
Called from `proof-done-advancing' when a save is processed and
`proof-last-theorem-dependencies' is set.

\(fn NAME GSPAN)" nil nil)

(autoload 'proof-dependency-in-span-context-menu "proof-depends" "\
Make some menu entries showing proof dependencies of SPAN.

\(fn SPAN)" nil nil)

;;;***

;;;### (autoloads (proof-easy-config) "proof-easy-config" "proof-easy-config.el"
;;;;;;  (22396 10805 379884 342000))
;;; Generated autoloads from proof-easy-config.el

(autoload 'proof-easy-config "proof-easy-config" "\
Configure Proof General for proof-assistant using BODY as a setq body.
The symbol SYM and string name NAME must match those given in
the `proof-assistant-table', which see.

\(fn SYM NAME &rest BODY)" nil t)

;;;***

;;;### (autoloads (proof-indent-line) "proof-indent" "proof-indent.el"
;;;;;;  (22396 10805 379884 342000))
;;; Generated autoloads from proof-indent.el

(autoload 'proof-indent-line "proof-indent" "\
Indent current line of proof script, if indentation enabled.

\(fn)" t nil)

;;;***

;;;### (autoloads nil "proof-maths-menu" "proof-maths-menu.el" (22432
;;;;;;  57744 776234 921000))
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
in future if we have just activated it for this buffer.

\(fn)" t nil)

;;;***

;;;### (autoloads nil "proof-menu" "proof-menu.el" (22432 57724 160233
;;;;;;  802000))
;;; Generated autoloads from proof-menu.el

(autoload 'proof-menu-define-keys "proof-menu" "\
Prover specific keymap under C-c C-a.

\(fn MAP)" nil nil)

(autoload 'proof-menu-define-main "proof-menu" "\


\(fn)" nil nil)

(autoload 'proof-menu-define-specific "proof-menu" "\


\(fn)" nil nil)

(autoload 'proof-aux-menu "proof-menu" "\
Construct and return PG auxiliary menu used in non-scripting buffers.

\(fn)" nil nil)

;;;***

;;;### (autoloads nil "proof-mmm" "proof-mmm.el" (22432 57506 976222
;;;;;;  13000))
;;; Generated autoloads from proof-mmm.el

(autoload 'proof-mmm-set-global "proof-mmm" "\
Set global status of MMM mode for PG buffers to be FLAG.

\(fn FLAG)" nil nil)

(autoload 'proof-mmm-enable "proof-mmm" "\
Turn on or off MMM mode in Proof General script buffer.
This invokes `mmm-mode' to toggle the setting for the current
buffer, and then sets PG's option for default to match.
Also we arrange to have MMM mode turn itself on automatically
in future if we have just activated it for this buffer.

\(fn)" t nil)

;;;***

;;;### (autoloads nil "proof-queue" "proof-queue.el" (22425 3273
;;;;;;  0 0))
;;; Generated autoloads from proof-queue.el

(autoload 'proof-start-queue "proof-queue" "\
Begin processing a oqueue of commands in QUEUEITEMS.
If START is non-nil, START and END are buffer positions in the
active scripting buffer for the queue region.

This function calls `proof-add-to-queue'.

\(fn START END QUEUEITEMS &optional QUEUEMODE)" nil nil)

(autoload 'proof-extend-queue "proof-queue" "\
Extend the current queue with QUEUEITEMS, queue end END.
To make sense, the commands should correspond to processing actions
for processing a region from (buffer-queue-or-locked-end) to END.
The queue mode is set to 'advancing

\(fn END QUEUEITEMS)" nil nil)

;;;***

;;;### (autoloads nil "proof-resolver" "proof-resolver.el" (22432
;;;;;;  52752 831963 959000))
;;; Generated autoloads from proof-resolver.el

(autoload 'proof-ready-prover "proof-resolver" "\


\(fn &optional QUEUEMODE)" nil nil)

(autoload 'proof-invisible-command "proof-resolver" "\


\(fn CMD &optional WAIT INVISIBLECALLBACK &rest FLAGS)" nil nil)

(autoload 'proof-invisible-cmd-get-result "proof-resolver" "\


\(fn CMD)" nil nil)

(autoload 'proof-invisible-command-invisible-result "proof-resolver" "\


\(fn CMD)" nil nil)

(autoload 'proof-add-to-queue "proof-resolver" "\


\(fn QUEUEITEMS &optional QUEUEMODE)" nil nil)

(autoload 'proof-server-interruptp "proof-resolver" "\


\(fn RESP)" nil nil)

(autoload 'proof-server-errorp "proof-resolver" "\


\(fn RESP)" nil nil)

(autoload 'proof-server-proof-completedp "proof-resolver" "\


\(fn RESP)" nil nil)

(autoload 'proof-server-send-to-prover "proof-resolver" "\


\(fn STRING-OR-FUN)" nil nil)

(autoload 'proof-server-process-response "proof-resolver" "\


\(fn RESP)" nil nil)

;;;***

;;;### (autoloads nil "proof-script" "proof-script.el" (22432 57475
;;;;;;  912220 327000))
;;; Generated autoloads from proof-script.el

(autoload 'proof-colour-locked "proof-script" "\
Alter the colour of all locked regions according to variable `proof-colour-locked'.

\(fn)" t nil)

(autoload 'proof-unprocessed-begin "proof-script" "\
Return end of locked region in current buffer or (point-min) otherwise.
The position is actually one beyond the last locked character.

\(fn)" nil nil)

(autoload 'proof-locked-region-full-p "proof-script" "\
Non-nil if the locked region covers all the buffer's non-whitespace.
Works on any buffer.

\(fn)" nil nil)

(autoload 'proof-locked-region-empty-p "proof-script" "\
Non-nil if the locked region is empty.  Works on any buffer.

\(fn)" nil nil)

(autoload 'pg-set-span-helphighlights "proof-script" "\
Add a daughter help span for SPAN with help message, highlight, actions.
The daughter span covers the non whitespace content of the main span.

We add the last output (when non-empty) to the hover display, and
also as the 'response property on the span.

Optional argument MOUSEFACE means use the given face as a mouse highlight
face, if it is a face, otherwise, if it is non-nil but not a face,
do not add a mouse highlight.

In any case, a mouse highlight and tooltip are only set if
`proof-output-tooltips' is non-nil.

Argument FACE means add 'face property FACE to the span.

\(fn SPAN &optional MOUSEFACE FACE)" nil nil)

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

(autoload 'proof-script-generic-parse-find-comment-end "proof-script" "\
Find the end of the comment point is at the start of.  Nil if not found.

\(fn)" nil nil)

(autoload 'proof-insert-pbp-command "proof-script" "\
Insert CMD into the proof queue.

\(fn CMD)" nil nil)

(autoload 'proof-insert-sendback-command "proof-script" "\
Insert CMD into the proof script, execute assert-until-point.

\(fn CMD)" nil nil)

(autoload 'proof-mode "proof-script" "\
Proof General major mode class for proof scripts.
\\{proof-mode-map}

\(fn)" t nil)

(autoload 'proof-config-done "proof-script" "\
Finish setup of Proof General scripting mode.
Call this function in the derived mode for the proof assistant to
finish setup which depends on specific proof assistant configuration.

\(fn)" nil nil)

;;;***

;;;### (autoloads (proof-server-exec-loop proof-server-add-to-queue
;;;;;;  proof-server-insert proof-server-invisible-command-invisible-result
;;;;;;  proof-server-invisible-cmd-get-result proof-server-invisible-command
;;;;;;  proof-server-start proof-server-ready-prover proof-server-config-done)
;;;;;;  "proof-server" "proof-server.el" (22422 12749 317662 164000))
;;; Generated autoloads from proof-server.el

(defsubst proof-server-live-buffer nil "\
Return non-nil if proof-server-buffer is live." (and proof-server-buffer (buffer-live-p proof-server-buffer) (let ((proc (get-buffer-process proof-server-buffer))) (and proc (memq (process-status proc) (quote (open run stop)))))))

(autoload 'proof-server-config-done "proof-server" "\
Initialise the specific prover after the child has been configured.
When using server mode, should call this function at the end of processing. 
For shell modes, the config-done procedure is called when instantiating an 
derived Emacs mode; here, we call the procedure directly.

\(fn)" nil nil)

(autoload 'proof-server-ready-prover "proof-server" "\
Compare with proof-shell-ready-prover, for proof shells. 
Make sure the proof assistant is ready for a command.
We ignore QUEUEMODE, which is used just to give calling compatibility 
with proof-shell-ready-prover.

\(fn QUEUEMODE)" nil nil)

(autoload 'proof-server-start "proof-server" "\


\(fn)" t nil)

(autoload 'proof-server-invisible-command "proof-server" "\


\(fn CMD &optional WAIT INVISIBLECALLBACK &rest FLAGS)" nil nil)

(autoload 'proof-server-invisible-cmd-get-result "proof-server" "\


\(fn CMD)" nil nil)

(autoload 'proof-server-invisible-command-invisible-result "proof-server" "\


\(fn CMD)" nil nil)

(autoload 'proof-server-insert "proof-server" "\
Send STRINGS to the prover.

STRINGS is a list of strings (which will be concatenated), or a
single string.

The ACTION is unused here (hangover from proof-shell-insert), 
while SPAN is the Emacs span containing the command.

\(fn STRINGS ACTION SPAN)" nil nil)

(autoload 'proof-server-add-to-queue "proof-server" "\
add item to queue for 'server mode

\(fn QUEUEITEMS &optional QUEUEMODE)" nil nil)

(autoload 'proof-server-exec-loop "proof-server" "\
Main loop processing the `proof-action-list', called from server process filter.

`proof-action-list' contains a list of (SPAN COMMAND ACTION [FLAGS]) lists.

If this function is called with a non-empty `proof-action-list', the
head of the list is the previously executed command which succeeded.
We execute the callback (ACTION SPAN) on the first item,
then (ACTION SPAN) on any following items which have null as
their cmd components.

If a there is a next command after that, send it to the process.

If the action list becomes empty, unlock the process and remove
the queue region.

The return value is non-nil if the action list is now empty or
contains only invisible elements for Prooftree synchronization.

\(fn)" nil nil)

;;;***

;;;### (autoloads nil "proof-site" "proof-site.el" (22426 33220 0
;;;;;;  0))
;;; Generated autoloads from proof-site.el

(autoload 'proof-ready-for-assistant "proof-site" "\
Configure PG for symbol ASSISTANTSYM, name ASSISTANT-NAME.
If ASSISTANT-NAME is omitted, look up in `proof-assistant-table'.

\(fn ASSISTANTSYM &optional ASSISTANT-NAME)" nil nil)

;;;***

;;;### (autoloads (proof-splash-message proof-splash-display-screen)
;;;;;;  "proof-splash" "proof-splash.el" (22396 10805 379884 342000))
;;; Generated autoloads from proof-splash.el

(autoload 'proof-splash-display-screen "proof-splash" "\
Save window config and display Proof General splash screen.
If TIMEOUT is non-nil, time out outside this function, definitely
by end of configuring proof mode.  Otherwise, make a key
binding to remove this buffer.

\(fn &optional TIMEOUT)" t nil)

(autoload 'proof-splash-message "proof-splash" "\
Make sure the user gets welcomed one way or another.

\(fn)" t nil)

;;;***

;;;### (autoloads (proof-format) "proof-syntax" "proof-syntax.el"
;;;;;;  (22396 10805 379884 342000))
;;; Generated autoloads from proof-syntax.el

(defsubst proof-replace-regexp-in-string (regexp rep string) "\
Like replace-regexp-in-string, but set case-fold-search to proof-case-fold-search." (let ((case-fold-search proof-case-fold-search)) (replace-regexp-in-string regexp rep string)))

(autoload 'proof-format "proof-syntax" "\
Format a string by matching regexps in ALIST against STRING.
ALIST contains (REGEXP . REPLACEMENT) pairs where REPLACEMENT
may be a string or sexp evaluated to get a string.

\(fn ALIST STRING)" nil nil)

;;;***

;;;### (autoloads (proof-toolbar-scripting-menu proof-toolbar-setup)
;;;;;;  "proof-toolbar" "proof-toolbar.el" (22396 10818 503884 55000))
;;; Generated autoloads from proof-toolbar.el

(autoload 'proof-toolbar-setup "proof-toolbar" "\
Initialize Proof General toolbar and enable it for all PG buffers.
If `proof-toolbar-enable' is nil, change the buffer toolbars
back the default toolbar.

\(fn)" t nil)
 (autoload 'proof-toolbar-toggle "proof-toolbar")

(autoload 'proof-toolbar-scripting-menu "proof-toolbar" "\
Menu made from the Proof General toolbar commands.

\(fn)" nil nil)

;;;***

;;;### (autoloads nil "proof-unicode-tokens" "proof-unicode-tokens.el"
;;;;;;  (22432 57297 780210 658000))
;;; Generated autoloads from proof-unicode-tokens.el

(autoload 'proof-unicode-tokens-mode-if-enabled "proof-unicode-tokens" "\
Turn on or off the Unicode Tokens minor mode in this buffer.

\(fn)" nil nil)

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
is changed.

\(fn)" t nil)

;;;***

;;;### (autoloads (proof-debug) "proof-utils" "proof-utils.el" (22396
;;;;;;  10818 503884 55000))
;;; Generated autoloads from proof-utils.el

(autoload 'proof-debug "proof-utils" "\
Issue the debugging message (format MSG ARGS) in the *PG Debug* buffer.
If flag `proof-general-debug' is nil, do nothing.

\(fn MSG &rest ARGS)" nil nil)

;;;***

;;;### (autoloads nil "../lib/texi-docstring-magic" "../lib/texi-docstring-magic.el"
;;;;;;  (22432 56527 596168 853000))
;;; Generated autoloads from ../lib/texi-docstring-magic.el

(autoload 'texi-docstring-magic "../lib/texi-docstring-magic" "\
Update all texi docstring magic annotations in buffer.
With prefix arg, no errors on unknown symbols.  (This results in
@def .. @end being deleted if not known).

\(fn &optional NOERROR)" t nil)

;;;***

;;;### (autoloads (unicode-chars-list-chars) "../lib/unicode-chars"
;;;;;;  "../lib/unicode-chars.el" (22396 10805 391884 342000))
;;; Generated autoloads from ../lib/unicode-chars.el

(autoload 'unicode-chars-list-chars "../lib/unicode-chars" "\
Insert each Unicode character into a buffer.
Lets you see which characters are available for literal display
in your emacs font.

\(fn)" t nil)

;;;***

;;;### (autoloads nil "../lib/unicode-tokens" "../lib/unicode-tokens.el"
;;;;;;  (22432 57207 956205 783000))
;;; Generated autoloads from ../lib/unicode-tokens.el

(autoload 'unicode-tokens-encode-str "../lib/unicode-tokens" "\
Return a unicode encoded version presentation of STR.

\(fn STR)" nil nil)

;;;***

;;;### (autoloads nil nil ("../lib/local-vars-list.el" "../lib/pg-fontsets.el"
;;;;;;  "../lib/proof-compat.el" "../lib/span.el" "pg-autotest.el"
;;;;;;  "pg-custom.el" "pg-pbrpm.el" "pg-vars.el" "proof-auxmodes.el"
;;;;;;  "proof-buffers.el" "proof-config.el" "proof-faces.el" "proof-proverargs.el"
;;;;;;  "proof-tree.el" "proof-useropts.el" "proof.el") (22432 58437
;;;;;;  876219 54000))

;;;***

;; Local Variables:
;; version-control: never
;; no-update-autoloads: t
;; End:

;;; proof-autoloads.el ends here
