;;; proof-config.el --- Proof General configuration for proof assistant

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
;; This file declares all prover-specific configuration variables for
;; Proof General.  The variables are used variously by the proof
;; script mode and the proof shell mode, menus, and toolbar.
;;
;; To customize Proof General for a new proof assistant, you
;; should read this file carefully!
;;
;;  1. Menus, user-level commands, toolbar
;;  2. Script mode configuration
;;  3. Interaction mode
;;  4. Server mode configuration
;;  5. Goals buffer configuration
;;
;; The remaining variables in should be set for each proof assistant.
;; You don't need to set every variable for basic functionality;
;; consult the manual for details of which ones are important.
;;
;; Customization groups and structure (sections in brackets)
;;
;;  proof-general	      : Overall group
;;    proof-user-options      : User options for Proof General
;;				  (see proof-useropts.el)
;;    <ProverName>            : User options for proof assistant
;;				  (see pg-custom.el)
;;    <ProverName->-internals : Internal settings for proof assistant
;;				  (see pg-custom.el)
;;
;;  proof-general-internals  :  Internal settings of Proof General
;;    prover-config	     :  Configuration for proof assistant (1)
;;      proof-script	     :     settings for proof script mode (2)
;;      proof-shell	     :     settings for proof shell mode (3)
;;      proof-goals	     :     settings for goals buffer (4)
;;    <Prover name>-config   :  Specific internal settings for a prover
;;
;; ======================================================================
;;
;; Developer notes:
;;
;; i. When adding a new configuration variable, please
;;    (a) put it in the right customize group, and
;;    (b) add a magical comment in ProofGeneral.texi/PG-adapting.texi
;;
;; ii.  Presently the customize library seems a bit picky over the
;;	:type property and some correct but complex types don't work:
;;	If the type is ill-formed, editing the whole group will be
;;	broken.  Check after updates, by killing all customize buffers
;;	and invoking customize-group
;;
;;
;; See also:
;;
;;  pg-custom.el
;;  pg-vars.el


;;; Code:

(require 'proof-useropts)		; user options
(require 'proof-faces)			; user options: faces
(eval-when-compile
  (require 'custom))

;;
;; Prelude
;;

(defgroup prover-config nil
  "Configuration of Proof General for the prover in use."
  :group 'proof-general-internals
  :prefix "proof-")

;; The variables in the "prover-config" (NB: not "proof config"!!)
;; customize group are those which are intended to be set by the
;; prover specific elisp, i.e. constants set on a per-prover basis.

;; Putting these in a customize group is useful for documenting this
;; type of variable, and for developing a new instantiation of Proof
;; General.  But it is *not* useful for final user-level
;; customization!  The reason is that saving these customizations
;; across a session is not liable to work, because the prover specific
;; elisp usually overrides with a series of setq's in
;; <assistant>-mode-config type functions.  This is why prover-config
;; appears under the proof-general-internal group.

(defcustom proof-guess-command-line nil
  "Function to guess command line for proof assistant, given a filename.
The function could take a filename as argument, run `make -n' to see
how to compile the file non-interactively, then translate the result
into an interactive invocation of the proof assistant with the same
command line options.  For an example, see coq/coq.el."
  :type 'function
  :group 'prover-config)



;;
;; 1. Configuration for menus, user-level commands, toolbar, etc.
;;

(defcustom proof-assistant-home-page ""
  "Web address for information on proof assistant.
Used for Proof General's help menu."
  :type 'string
  :group 'prover-config)

(defcustom proof-context-command nil
  "Command to display the context in proof assistant."
  :type 'function
  :group 'prover-config)

(defcustom proof-info-command nil
  "Command to ask for help or information in the proof assistant.
String or fn.  If a string, the command to use.
If a function, it should return the command string to insert."
  :type '(choice string function)
  :group 'prover-config)

(defcustom proof-showproof-command nil
  "Command to display proof state in proof assistant."
  :type 'string
  :group 'prover-config)

(defcustom proof-goal-command nil
  "Command to set a goal in the proof assistant.  String or fn.
If a string, the format character `%s' will be replaced by the
goal string.
If a function, it should return the command string to insert."
  :type '(choice string function)
  :group 'prover-config)

(defcustom proof-save-command nil
  "Command to save a proved theorem in the proof assistant.  String or fn.
If a string, the format character `%s' will be replaced by the
theorem name.
If a function, it should return the command string to insert."
  :type '(choice string function)
  :group 'prover-config)

(defcustom proof-check-command nil
  "Command to check the validity of all processed proofs in the current
document."
  :type 'function
  :group 'prover-config)

(defcustom proof-command-formatting-fun nil
  "Function to format commands to send to prover."
  :type 'function
  :group 'prover-config)

(defcustom proof-find-theorems-command nil
  "Command to search for a theorem containing a given term.  String or fn.
If a string, the format character `%s' will be replaced by the term.
If a function, it should return the command string to send."
  :type '(choice string function)
  :group 'prover-config)

(defcustom proof-query-identifier-command nil
  "Command sent to the prover to query about a given identifier (or string).
This is typically a command used to print a theorem, constant, or whatever.
Inside the command the string %s is replaced by the given identifier or
string.

Value should be a string for a single command, or maybe an association
list between values for `proof-buffer-syntactic-context' and strings,
which allows different prover commands to be sent based on the syntactic
context of the string.
If value is an alist, must include a default value for no context (nil)."
  :type '(or string (list (cons (choice 'nil 'string 'comment)  string)))
  :group 'prover-config)

(defcustom proof-assistant-true-value "true"
  "String for true values in proof assistant, used for setting flags.
Default is the string \"true\"."
  :type 'string
  :group 'prover-config)

(defcustom proof-assistant-false-value "false"
  "String for false values in proof assistant, used for setting flags.
Default is the string \"false\"."
  :type 'string
  :group 'prover-config)

(defcustom proof-assistant-format-int-fn 'int-to-string
  "Function for converting integer values to ints in proof assistant.
Used for configuring settings in proof assistant.
Default is `int-to-string'."
  :type 'function
  :group 'prover-config)

(defcustom proof-assistant-format-float-fn 'number-to-string
  "Function for converting float values to ints in proof assistant.
Used for configuring settings in proof assistant.
Default is `number-to-string'."
  :type 'function
  :group 'prover-config)

(defcustom proof-assistant-format-string-fn  (lambda (value) value)
  "Function for converting string values to strings in proof assistant.
Used for configuring settings in proof assistant.
Default is the identity function."
  :type 'string
  :group 'prover-config)

(defcustom proof-assistant-setting-format nil
  "Function for formatting setting strings for proof assistant.
Setting strings are calculated by replacing a format character
%b, %i, or %s in the :setting string in for each variable defined with
`defpacustom', using the current value of that variable.  This
function  is applied as a final step to do any extra markup, or
conversion, etc.  (No changes are done if nil)."
  :type '(choice string (const nil))
  :group 'prover-config)

(defcustom proof-tree-configured nil
  "Whether external proof-tree display is configured.
This boolean enables the proof-tree menu entry and the function
that starts external proof-tree display."
  :type 'boolean
  :group 'proof-tree-internals)


;;
;; 2. Configuration for proof script mode
;;

;;
;; The following variables should be set before proof-config-done
;; is called.  These configure the mode for the script buffer,
;; including highlighting, etc.
;;

(defgroup proof-script nil
  "Proof General configuration of scripting buffer mode."
  :group 'prover-config
  :prefix "proof-")

(defcustom proof-terminal-string nil
  "String that terminates commands sent to prover; nil if none.

To configure command recognition properly, you must set at least one
of these: `proof-script-sexp-commands', `proof-script-command-end-regexp',
`proof-script-command-start-regexp', `proof-terminal-string',
or `proof-script-parse-function'."
  :type 'character
  :group 'prover-config)

(defcustom proof-electric-terminator-noterminator nil
  "If non-nil, electric terminator does not actually insert a terminator."
  :type 'boolean
  :group 'prover-config)

(defcustom proof-script-sexp-commands nil
  "Non-nil if script has LISP-like syntax: commands are top-level sexps.
You should set this variable in script mode configuration.

To configure command recognition properly, you must set at least one
of these: `proof-script-sexp-commands', `proof-script-command-end-regexp',
`proof-script-command-start-regexp', `proof-terminal-string',
or `proof-script-parse-function'."
  :type 'boolean
  :group 'prover-config)

(defcustom proof-script-command-end-regexp nil
  "Regular expression which matches end of commands in proof script.
You should set this variable in script mode configuration.

The end of the command is considered to be the end of the match
of this regexp.  The regexp may include a nested group, which
can be used to recognize the start of the following command
\(or white space).  If there is a nested group, the end of the
command is considered to be the start of the nested group,
i.e. (match-beginning 1), rather than (match-end 0).

To configure command recognition properly, you must set at least one
of these: `proof-script-sexp-commands', `proof-script-command-end-regexp',
`proof-script-command-start-regexp', `proof-terminal-string',
or `proof-script-parse-function'."
  :type 'string
  :group 'prover-config)

(defcustom proof-script-command-start-regexp nil
  "Regular expression which matches start of commands in proof script.
You should set this variable in script mode configuration.

To configure command recognition properly, you must set at least one
of these: `proof-script-sexp-commands', `proof-script-command-end-regexp',
`proof-script-command-start-regexp', `proof-terminal-string',
or `proof-script-parse-function'."
  :type 'string
  :group 'prover-config)

(defcustom proof-script-trim-spans nil
  "Whether to trim whitespace from the start of spans created for script
items"
  :type 'boolean
  :group 'prover-config)

(defcustom proof-script-integral-proofs nil
  "Whether the complete text after a goal confines the actual proof.

In structured proof languages like Isabelle/Isar a theorem is
established by a goal statement (with full information about the
result, including name and statement), followed by a self-contained
piece of text for the proof.  The latter should be treated as an
integral entity for purposes of hiding proof bodies etc.

This variable is better set to nil for tactical provers (like Coq)
where important information about the result is spread over the
initial ``goal'' and the final ``save'' command."
  :type 'boolean
  :group 'prover-config)

(defcustom proof-script-fly-past-comments nil
  "*If non-nil, fly past successive comments, coalescing into single spans."
  :type 'boolean
  :group 'proof-user-options)

(defcustom proof-script-parse-function nil
  "A function which parses a portion of the proof script.
It is called with the proof script as the current buffer, and
point the position where the parse should begin.  It should
move point to the exact end of the next \"segment\", and return
a symbol indicating what has been parsed:

  'comment	for a comment
  'cmd		for a proof script command
  nil		if there is no complete next segment in the buffer

If this is left unset, it will be configured automatically to
a generic function according to which of `proof-terminal-string'
and its friends are set."
  :type 'string
  :group 'prover-config)


(defcustom proof-script-comment-start ""
  "String which starts a comment in the proof assistant command language.
The script buffer's `comment-start' is set to this string plus a space.
Moreover, comments are usually ignored during script management, and not
sent to the proof process.

You should set this variable for reliable working of Proof General,
as well as `proof-script-comment-end'."
  :type 'string
  :group 'proof-script)

(defcustom proof-script-comment-start-regexp nil
  "Regexp which matches a comment start in the proof command language.

The default value for this is set as (regexp-quote `proof-script-comment-start')
but you can set this variable to something else more precise if necessary."
  :type 'string
  :group 'proof-script)

(defcustom proof-script-comment-end ""
  "String which ends a comment in the proof assistant command language.
Should be an empty string if comments are terminated by `end-of-line'
The script buffer's `comment-end' is set to a space plus this string,
if it is non-empty.

See also `proof-script-comment-start'.

You should set this variable for reliable working of Proof General."
  :type 'string
  :group 'proof-script)

(defcustom proof-script-comment-end-regexp nil
  "Regexp which matches a comment end in the proof command language.

The default value for this is set as (regexp-quote `proof-script-comment-end')
but you can set this variable to something else more precise if necessary."
  :type 'string
  :group 'proof-script)

(defcustom proof-string-start-regexp "\""
  "Matches the start of a quoted string in the proof assistant command language."
  :type 'string
  :group 'proof-script)

(defcustom proof-string-end-regexp "\""
  "Matches the end of a quoted string in the proof assistant command language."
  :type 'string
  :group 'proof-script)

(defcustom proof-case-fold-search nil
  "Value for `case-fold-search' when recognizing portions of proof scripts.
Also used for completion, via `proof-script-complete'.
The default value is nil.  If your prover has a case *insensitive*
input syntax, `proof-case-fold-search' should be set to t instead.
NB: This setting is not used for matching output from the prover."
  :type 'boolean :group
  'proof-script)

(defcustom proof-save-with-hole-regexp nil
  "Regexp which matches a command to save a named theorem.
The name of the theorem is built from the variable
`proof-save-with-hole-result' using the same convention as
`query-replace-regexp'.
Used for setting names of goal..save and proof regions.

It's safe to leave this setting as nil."
  :type 'regexp
  :group 'proof-script)

(defcustom proof-save-with-hole-result 2
  "How to get theorem name after `proof-save-with-hole-regexp' match.
String or Int.
If an int N use match-string to recover the value of the Nth parenthesis matched.
If it is a string use replace-match. In this case, `proof-save-with-hole-regexp'
should match the entire command"
  :type '(choice string integer)
  :group 'proof-script)

;; FIXME: unify uses so that proof-anchor-regexp works sensibly
(defcustom proof-goal-command-regexp nil
  "Matches a goal command in the proof script.
This is used to make the default value for `proof-goal-command-p',
used as an important part of script management to find the start
of an atomic undo block."
  :type 'regexp
  :group 'proof-script)

(defcustom proof-goal-with-hole-regexp nil
  "Regexp which matches a command used to issue and name a goal.
The name of the theorem is built from the variable
`proof-goal-with-hole-result' using the same convention as
for `query-replace-regexp'.
Used for setting names of goal..save regions and for default
configuration of other modes (function menu, imenu).

It's safe to leave this setting as nil."
  :type 'regexp
  :group 'proof-script)

(defcustom proof-goal-with-hole-result 2
  "How to get theorem name after `proof-goal-with-hole-regexp' match.
String or Int.
If an int N use match-string to recover the value of the Nth parenthesis matched.
If it is a string use replace-match. In this case, proof-save-with-hole-regexp
should match the entire command"
  :type '(choice string integer)
  :group 'proof-script)

(defcustom proof-non-undoables-regexp nil
  "Regular expression matching commands which are *not* undoable.
These are commands which should not appear in proof scripts,
for example, undo commands themselves (if the proof assistant
cannot \"redo\" an \"undo\").
Used in default functions `proof-generic-state-preserving-p'
and `proof-generic-count-undos'.  If you don't use those,
may be left as nil."
  :type '(choice (const nil) regexp)
  :group 'proof-script)

(defcustom proof-nested-undo-regexp nil
  "Regexp for commands that must be counted in nested goal-save regions.

Used for provers which allow nested atomic goal-saves, but with some
nested history that must be undone specially.

At the moment, the behaviour is that a goal-save span has a 'nestedundos
property which is set to the number of commands within it which match
this regexp.  The idea is that the prover-specific code can create a
customized undo command to retract the goal-save region, based on the
'nestedundos setting.  Coq uses this to forget declarations, since
declarations in Coq reside in a separate context with its own (flat)
history."
  :type '(choice (const nil) regexp)
  :group 'proof-script)

(defcustom proof-ignore-for-undo-count nil
  "Matcher for script commands to be ignored in undo count.
May be left as nil, in which case it will be set to
`proof-non-undoables-regexp'.
Used in default function `proof-generic-count-undos'."
  :type '(choice (const nil) regexp function)
  :group 'proof-script)

(defcustom proof-script-imenu-generic-expression nil
  "Regular expressions to help find definitions and proofs in a script.
Value for `imenu-generic-expression', see documentation of Imenu
and that variable for details."
  :type 'sexp
  :group 'proof-script)

(defcustom proof-goal-command-p 'proof-generic-goal-command-p
  "A function to test: is this really a goal command span?

This is added as a more refined addition to `proof-goal-command-regexp',
to solve the problem that Coq and some other provers can have goals which
look like definitions, etc.  (In the future we may generalize
`proof-goal-command-regexp' instead)."
  :type 'function
  :group 'proof-script)

(defcustom proof-really-save-command-p (lambda (span cmd) t)
  "Is this really a save command?

This is a more refined addition to `proof-save-command-regexp'.
It should be a function taking a span and command as argument,
and can be used to track nested proofs."
  :type 'function
  :group 'proof-script)

(defcustom proof-completed-proof-behaviour nil
  "Indicates how Proof General treats commands beyond the end of a proof.
Normally goal...save regions are \"closed\", i.e. made atomic for undo.
But once a proof has been completed, there may be a delay before
the \"save\" command appears --- or it may not appear at all.  Unless
nested proofs are supported, this can spoil the undo-behaviour in
script management since once a new goal arrives the old undo history
may be lost in the prover.  So we allow Proof General to close
off the goal..[save] region in more flexible ways.
The possibilities are:

	nil  -  nothing special; close only when a save arrives
  'closeany  -  close as soon as the next command arrives, save or not
 'closegoal  -  close when the next \"goal\" command arrives
    'extend  -  keep extending the closed region until a save or goal.

If your proof assistant allows nested goals, it will be wrong to close
off the portion of proof so far, so this variable should be set to nil.

NB: 'extend behaviour is not currently compatible with appearance of
save commands, so don't use that if your prover has save commands."
  :type '(choice
	  (const :tag "Close on save only" nil)
	  (const :tag "Close next command" closeany)
	  (const :tag "Close next goal" closegoal)
	  (const :tag "Extend" ignore))
  :group 'proof-script)

(defcustom proof-count-undos-fn 'proof-generic-count-undos
  "Function to calculate a list of commands to undo to reach a target span.
The function takes a span as an argument, and should return a string
which is the command to undo to the target span.  The target is
guaranteed to be within the current (open) proof.
This is an important function for script management.
The default setting `proof-generic-count-undos' is based on the
settings `proof-non-undoables-regexp' and
`proof-non-undoables-regexp'."
  :type 'function
  :group 'proof-script)

(defcustom proof-find-and-forget-fn 'proof-generic-find-and-forget
  "Function to return list of commands to forget to before its argument span.
This setting is used to for retraction (undoing) in proof scripts.

It should undo the effect of all settings between its target span
up to (proof-unprocessed-begin).  This may involve forgetting a number
of definitions, declarations, or whatever.

If return value is nil, it means there is nothing to do.

This is an important function for script management.
Study one of the existing instantiations for examples of how to write it,
or leave it set to the default function `proof-generic-find-and-forget'
\(which see)."
  :type 'function
  :group 'proof-script)

(defcustom proof-forget-id-command nil
  "Command to forget back to a given named span.
A string; `%s' will be replaced by the name of the span.

This is only used in the implementation of `proof-generic-find-and-forget',
you only need to set if you use that function (by not customizing
`proof-find-and-forget-fn'."
  :type 'string
  :group 'proof-script)

(defcustom proof-kill-goal-command nil
  "Command to kill the currently open goal.

If this is set to nil, PG will expect `proof-find-and-forget-fn'
to do all the work of retracting to an arbitrary point in a file.
Otherwise, the generic split-phase mechanism will be used:

1. If inside an unclosed proof, use `proof-count-undos'.
2. If retracting to before an unclosed proof, use
`proof-kill-goal-command', followed by `proof-find-and-forget-fn'
if necessary."
  :type 'string
  :group 'proof-script)

(defcustom proof-undo-n-times-cmd nil
  "Command to undo n steps of the currently open goal.
String or function.
If this is set to a string, `%s' will be replaced by the number of
undo steps to issue.
If this is set to a function, it should return a list of
the appropriate commands (given the number of undo steps).

This setting is used for the default `proof-generic-count-undos'.
If you set `proof-count-undos-fn' to some other function, there is no
need to set this variable."
  :type '(or string function)
  :group 'proof-script)

(defcustom proof-nested-goals-history-p nil
  "Whether the prover supports recovery of history for nested proofs.
If it does (non-nil), Proof General will retain history inside
nested proofs.
If it does not, Proof General will amalgamate nested proofs into single
steps within the outer proof."
  :type 'boolean
  :group 'proof-script)

(defcustom proof-arbitrary-undo-positions nil
  "Non-nil if Proof General may undo to arbitrary positions.
The classic behaviour of Proof General is to undo completed
proofs in one step: this design arose because older provers
discarded nested history once proofs were complete.  The proof
script engine amalgamates spans for a complete proof (into a
single 'goalsave) to give this effect.

Newer designs keep more state, and may support arbitrary undo
with a file being processed.  If this flag is non-nil,
amalgamation will not happen."
  :type 'boolean
  :group 'proof-script)

(defcustom proof-state-preserving-p 'proof-generic-state-preserving-p
  "A predicate, non-nil if its argument (a command) preserves the proof state.
This is a safety-test used by `proof-minibuffer-cmd' to filter out scripting
commands which should be entered directly into the script itself.

The default setting for this function, `proof-generic-state-preserving-p'
tests by negating the match on `proof-non-undoables-regexp'."
  :type 'function
  :group 'proof-script)

(defcustom proof-activate-scripting-hook nil
  "Hook run when a buffer is switched into scripting mode.
The current buffer will be the newly active scripting buffer.

This hook may be useful for synchronizing with the proof
assistant, for example, to switch to a new theory
\(in case that isn't already done by commands in the proof
script).

When functions in this hook are called, the variable
`activated-interactively' will be non-nil if
`proof-activate-scripting' was called interactively
\(rather than as a side-effect of some other action).
If a hook function sends commands to the proof process,
it should wait for them to complete (so the queue is cleared
for scripting commands), unless activated-interactively is set."
  :type '(repeat function)
  :group 'proof-script)

(defcustom proof-deactivate-scripting-hook nil
  "Hook run when a buffer is switched out of scripting mode.
The current buffer will be the recently scripting buffer.

This hook may be useful for synchronizing with the proof
assistant, for example, to compile a completed file."
  :type '(repeat function)
  :group 'proof-script)

(defcustom proof-no-fully-processed-buffer nil
  "Set to t if buffers should always retract before scripting elsewhere.
Leave at nil if fully processed buffers make sense for the current
proof assistant. If nil the user can choose to fully assert a
buffer when starting scripting in a different buffer. If t there
is only the choice to fully retract the active buffer before
starting scripting in a different buffer. This last behavior is
needed for Coq."
  :type 'boolean
  :group 'proof-script)

(defcustom proof-script-evaluate-elisp-comment-regexp "ELISP: -- \\(.*\\) --"
  "Matches text within a comment telling Proof General to evaluate some code.
This allows Emacs Lisp to be executed during scripting.
\(It's also a fantastic backdoor security risk).

If the regexp matches text inside a comment, there should be
one subexpression match string, which will contain elisp code
to be evaluated.

Elisp errors will be trapped when evaluating; set
`proof-general-debug' to be informed when this happens."
  :type 'regexp
  :group 'proof-script)

;;
;; Proof script indentation
;;

(defcustom proof-indent 2
  "Amount of proof script indentation."
  :type 'number
  :group 'proof-script)

(defcustom proof-indent-hang nil
  "Enable 'hanging' indentation for proof script."
  :type 'boolean
  :group 'proof-script)

(defcustom proof-indent-enclose-offset 1
  "Extra offset for enclosing indentation syntax elements."
  :type 'number
  :group 'proof-script)

(defcustom proof-indent-open-offset 1
  "Extra offset for opening indentation syntax elements."
  :type 'number
  :group 'proof-script)

(defcustom proof-indent-close-offset 1
  "Extra offset for closing indentation syntax elements."
  :type 'number
  :group 'proof-script)

(defcustom proof-indent-any-regexp "\\s(\\|\\s)"
  "Regexp for *any* syntax element guiding proof script indentation."
  :type 'string
  :group 'proof-script)

(defcustom proof-indent-inner-regexp nil
  "Regexp for text within syntax elements of proof script indentation."
  :type 'string
  :group 'proof-script)

(defcustom proof-indent-enclose-regexp nil
  "Regexp for enclosing syntax elements of proof script indentation."
  :type 'string
  :group 'proof-script)

(defcustom proof-indent-open-regexp "\\s("
  "Regexp for opening syntax elements of proof script indentation."
  :type 'string
  :group 'proof-script)

(defcustom proof-indent-close-regexp "\\s)"
  "Regexp for closing syntax elements of proof script indentation."
  :type 'string
  :group 'proof-script)

(defcustom proof-script-insert-newlines t
  "if non-nil inserts a newline between each message in response buffer."
  :type 'boolean
  :group 'proof-script)

(defcustom proof-script-color-error-messages t
  "if non-nil error messages will be globally colored with corresponding face.
If prover mode has a better coloring mechanism for errors, set this to nil."
  :type 'boolean
  :group 'proof-script)

(defcustom proof-script-font-lock-keywords nil
  "Value of `font-lock-keywords' used to fontify proof scripts.
The proof script mode should set this before calling `proof-config-done'.
See also `proof-goals-font-lock-keywords' and `proof-response-font-lock-keywords'."
  :type 'sexp
  :group 'proof-script)

;;
;; Proof script context menu customization
;;
(defcustom proof-script-span-context-menu-extensions nil
  "Extensions for the in-span context sensitive menu.
This should be a function which accepts three arguments: SPAN IDIOM NAME.
See pg-user.el: `pg-create-in-span-context-menu' for more hints."
  :type 'function
  :group 'proof-script)


;;
;;  3. Interaction mode
;;     
;;  Most provers will provide a REPL to Proof General, which looks for a REPL prompt 
;;   In that case, Proof General uses a proof shell customized for each prover
;;  Some provers (like Coq), also offer a client/server operation
;;   In that case, Proof General reads and writes data via pipes or sockets
;;  There are some regexps that are common to both modes

(defgroup proof-interaction nil
  "How Proof General interacts with the prover."
  :group 'prover-config
  :prefix "proof-interaction-")

; default is 'repl so most provers don't have to configure this
(defcustom proof-interaction-mode 'repl
  "REPL or server choice"
  :type 'symbol ; 'repl or 'server
  :group 'proof-interaction)

; for the following function variables, the default function is for 'repl mode
; so don't need to worry about them in configuration unless in 'server mode

(defcustom proof-ready-prover-fun 'proof-server-ready-prover
  "Function to see if prover ready"
  :type 'function
  :group 'proof-interaction)

(defcustom proof-invisible-command-fun 'proof-server-invisible-command
  "Function to send a command to server"
  :type 'function
  :group 'proof-interaction)

(defcustom proof-invisible-cmd-handle-result-fun 'proof-server-invisible-cmd-handle-result
  "Function to send a command to server, send result to handler"
  :type 'function
  :group 'proof-interaction)

(defcustom proof-invisible-command-invisible-result-fun 'proof-server-invisible-command-invisible-result
  "Function to send a command to server, save result"
  :type 'function
  :group 'proof-interaction)

(defcustom proof-add-to-queue-fun 'proof-server-add-to-queue
  "Function to add item to the queue of items to be processed by the proof server"
  :type 'function
  :group 'proof-interaction)

(defcustom proof-extend-queue-hook nil
  "Hooks run by proof-extend-queue before extending `proof-action-list'.
Can be used to run additional actions before items are added to
the queue \(such as compiling required modules for Coq) or to
modify the items that are going to be added to
`proof-action-list'. The items that are about to be added are
bound to `queueitems'."
  :type '(repeat function)
  :group 'proof-interaction)  

(defcustom proof-prover-unicode t
  "Whether communication between PG and prover is 8bit clean.
If non-nil, no special non-ASCII characters must be used in markup.
If so, the process coding system will be set to UTF-8.
With old systems that may use unsafe unicode prefix sequences
\(i.e., lead to hanging in C-libraries), this should be set to nil."
  :type 'boolean
  :group 'proof-interaction)


;; TODO: move proof shell and server common regexps here


;;
;;  4. Configuration for proof server
;;
;; The variables in this section concern the proof server mode.
;; The first group of variables are hooks invoked at various points.
;; The second group of variables are concerned with matching the output
;; from the proof assistant.
;;
;; Variables here are put into the customize group 'proof-server'.
;;
;; These should be set in the server mode configuration, again,
;; before proof-server-config-done is called.

(defgroup proof-server nil
  "Settings for output from the proof assistant in proof server mode."
  :group 'prover-config
  :prefix "proof-server-")

(defcustom proof-server-cd-cmd nil
  "Command to the proof assistant to change the working directory.
The format character `%s' is replaced with the directory, and
the escape sequences in `proof-server-filename-escapes' are
applied to the filename.

This setting is used to define the function `proof-cd' which
changes to the value of (default-directory) for script buffers.
For files, the value of (default-directory) is simply the
directory the file resides in.

NB: By default, `proof-cd' is called from `proof-activate-scripting-hook',
so that the prover switches to the directory of a proof
script every time scripting begins."
  :type 'string
  :group 'proof-server)

(defcustom proof-server-insert-hook nil
  "Hooks run by `proof-server-insert' before inserting a command.
Can be used to configure the proof assistant to the interface in
various ways -- for example, to observe or alter the commands sent to
the prover, or to sneak in extra commands to configure the prover.

The hook can massage `string' or insert additional
text.

Additionally, the hook can examine the variable `action'.  It will be
a symbol, set to the callback command which is executed in the proof
shell filter once `string' has been processed.  The `action' variable
suggests what class of command is about to be inserted, the first two
are normally the ones of interest:

 'proof-done-advancing	     A \"forward\" scripting command
 'proof-done-retracting	     A \"backward\" scripting command
 'proof-done-invisible	     A non-scripting command
 'proof-shell-set-silent     Indicates prover output has been surpressed
 'proof-shell-clear-silent   Indicates prover output has been restored
 'init-cmd	             Early initialization command sent to prover

See also `proof-script-preprocess' which can munge text when
it is added to the queue of commands."
  :type '(repeat function)
  :group 'proof-server)

(defcustom proof-prog-name nil
  "System command to run the proof assistant. 
May contain arguments separated by spaces, but see also the
prover specific settings `<PA>-prog-args' and `<PA>-prog-env'.

Remark: if `<PA>-prog-args' is non-nil, then `proof-prog-name' is considered
strictly: it must contain *only* the program name with no option, spaces
are interpreted literally as part of the program name."
  :type 'string
  :group 'proof-server)

(defcustom proof-server-trace-output-regexp nil
  "Matches tracing output which should be displayed in trace buffer.
Each line which matches this regexp but would otherwise be treated
as an ordinary response, is sent to the trace buffer instead of the
response buffer.

This is intended for unusual debugging output from
the prover, rather than ordinary output from final proofs.

This should match a string which is bounded by matches
on `proof-shell-eager-annotation-start' and
`proof-shell-eager-annotation-end'.

Set to nil to disable."
  :type '(choice (const nil) regexp)
  :group 'proof-server)

(defcustom pg-special-char-regexp "[\200-\377]"
  "Regexp matching any \"special\" character sequence."
  :type 'string
  :group 'proof-server)

;; FIXME FIXME: this next one not yet used.  It's hard to interleave
;; commands with the ordinary queue anyway: the prover should
;; automatically output this information if it is enabled.
'(defcustom proof-server-query-dependencies-cmd nil
  "Command to query the prover for dependencies of given theorem name.
%s is replaced by the name of the theorem.   This command will be
sent when a proof is completed."
  :type 'string
  :group 'proof-server)

(defcustom proof-server-theorem-dependency-list-regexp nil
  "Matches output telling Proof General about dependencies.
This is to allow navigation and display of dependency information.
The output from the prover should be a message with the form

   DEPENDENCIES OF  X Y Z   ARE  A B C

with X Y Z, A B C separated by whitespace or somehow else (see
`proof-server-theorem-dependency-list-split'.  This variable should
be set to a regexp to match the overall message (which should
be an urgent message), with two sub-matches for X Y Z and A B C.

This is an experimental feature, currently work-in-progress."
  :type '(choice (const nil) regexp)
  :group 'proof-server)

(defcustom proof-server-theorem-dependency-list-split nil
  "Splits strings which match `proof-server-theorem-dependency-list-regexp'.
Used as an argument to `split-string'; nil defaults to whitespace.
\(This setting is necessary for provers which allow whitespace in
the names of theorems/definitions/constants), see setting for
Isabelle in isa/isa.el and isar/isar.el."
  :type '(choice (const nil) regexp)
  :group 'proof-server)

(defcustom proof-server-show-dependency-cmd nil
  "Command sent to the prover to display a dependency.
This is typically a command used to print a theorem, constant, or whatever.
A string with %s replaced by the dependency name."
  :type 'string
  :group 'proof-server)

(defcustom proof-auto-multiple-files nil
  "Whether to use automatic multiple file management.
If non-nil, Proof General will automatically retract a script file
whenever another one is retracted which it depends on.  It assumes
a simple linear dependency between files in the order which
they were processed.

If your proof assistant has no management of file dependencies, or one
which depends on a simple linear context, you may be able to use this
setting to good effect.  If the proof assistant has more complex
file dependencies then you should configure it to communicate with
Proof General about the dependencies rather than using this setting."
  :type 'boolean
  :group 'proof-server) 

(defcustom proof-cannot-reopen-processed-files nil
  "Non-nil if the prover allows re-opening of already processed files.

If the user has used Proof General to process a file incrementally,
then PG will retain the spans recording undo history in the buffer
corresponding to that file (provided it remains visited in Emacs).

If the prover allows, it will be possible to undo to a position within
this file.  If the prover does *not* allow this, this variable should
be set non-nil, so that when a completed file is activated for
scripting (to do undo operations), the whole history is discarded."
  :type 'boolean
  :group 'proof-server)

(defcustom pg-next-error-regexp nil
  "Regular expression which matches an error message, perhaps with line/column.
Used by `proof-next-error' to jump to line numbers causing
errors during some batch processing of the proof assistant.
\(During \"manual\" script processing, script usually automatically
jumps to the end of the locked region)

Match number 2 should be the line number, if present.
Match number 3 should be the column number, if present.

The filename may be matched by `pg-next-error-filename-regexp',
which is assumed to precede pg-next-error-regexp."
  :type 'string
  :group 'proof-server)

(defcustom pg-next-error-filename-regexp nil
  "Used to locate a filename that an error message refers to.
Used by `proof-next-error' to jump to locations causing
errors during some batch processing of the proof assistant.
\(During \"manual\" script processing, the script usually automatically
jumps to the end of the locked region).

Match number 2 should be the file name, if present.

Errors must first be matched by `pg-next-error-regexp'
\(whether they contain a line number or not).  The response buffer
is then searched *backwards* for a regexp matching this variable,
`pg-next-error-filename-regexp'.  (So if the
filename appears after the line number, make the first regexp
match the whole line).  Finally
`pg-next-error-extract-filename'
may be used to extract the filename." 
  :type 'string
  :group 'proof-server)

;; FIXME: generalize this to string-or-function scheme
(defcustom pg-next-error-extract-filename nil
  "A string used to extract filename from error message.  %s replaced.
NB: this is only used if the match itself does not already correspond
to a filename."
  :type 'string
  :group 'proof-server)

(defcustom proof-server-strip-output-markup 'identity
  "A function which strips markup from the process output.
This should remove any markup which is made invisible by font-lock
when displayed in the output buffer.  This is used in
`pg-insert-last-output-as-comment' to insert output into the
proof script, and for cut and paste operations."
  :type 'function
  :group 'proof-server)

(defcustom  proof-server-inform-file-processed-cmd nil
 "Command to the proof assistant to tell it that a file has been processed.
The format character `%s' is replaced by a complete filename for a
script file which has been fully processed interactively with
Proof General.  See `proof-format-filename' for other possibilities
to process the filename.

This setting used to interface with the proof assistant's internal
management of multiple files, so the proof assistant is kept aware of
which files have been processed.  Specifically, when scripting
is deactivated in a completed buffer, it is added to Proof General's
list of processed files, and the prover is told about it by
issuing this command.

If this is set to nil, no command is issued.

See also: `proof-server-inform-file-retracted-cmd',
`proof-server-process-file', `proof-server-compute-new-files-list'."
 :type '(choice string (const nil))
 :group 'proof-server)

(defcustom  proof-server-inform-file-retracted-cmd nil
 "Command to the proof assistant to tell it that a file has been retracted.
The format character `%s' is replaced by a complete filename for a
script file which Proof General wants the prover to consider as not
completely processed.  See `proof-format-filename' for other
possibilities to process the filename.

This is used to interface with the proof assistant's internal
management of multiple files, so the proof assistant is kept aware of
which files have been processed.  Specifically, when scripting
is activated, the file is removed from Proof General's list of
processed files, and the prover is told about it by issuing this
command.  The action may cause the prover in turn to suggest to
Proof General that files depending on this one are
also unlocked.

If this is set to nil, no command is issued.

It is also possible to set this value to a function which will
be invoked on the name of the retracted file, and should remove
the ancestor files from `proof-included-files-list' by some
other calculation.

See also: `proof-server-inform-file-processed-cmd',
`proof-server-process-file', `proof-server-compute-new-files-list'."
 :type '(choice string (const nil)) ;; FIXME: or function
 :group 'proof-server)

(defcustom proof-server-send-to-prover-fun nil
   "The procedure to send something to the prover. This can be as simple as a wrapper around 
process-send-string, or it might use sockets or pipes. Takes optional flag indicating the 
data needs formatting before sending."
   :type 'function
   :group 'proof-server)

(defcustom proof-server-response-complete-fun nil
  "Have we received all pending responses from the prover. Used to know when we 
can undo the last command sent."
  :type 'function
  :group 'proof-server)

(defcustom proof-server-retract-buffer-hook nil
  "Code to run just after the buffer is retracted."
  :type 'function
  :group 'proof-server)

(defcustom proof-server-interrupt-fun nil
  "Code to interrupt the prover, if anything more than 
a SIGINT is needed."
  :type 'function
  :group 'proof-server)

(defcustom proof-server-make-command-thunk-fun nil
   "The procedure to thunkify a command for the prover."
   :type 'function
   :group 'proof-server)

(defcustom proof-server-init-cmd nil
   "The command(s) for initially configuring the proof process.
This command is sent to the process as soon as it started. It can be used to configure
the proof assistant in some way."
   :type '(choice (list string) string (const nil))
   :group 'proof-server)

(defcustom proof-server-quit-cmd nil
  "A command to quit the proof server process.  If nil, send EOF instead."
   :type '(choice string (const nil))
   :group 'proof-server)

(defcustom proof-server-process-response-fun nil
  "Take response from prover, parse it and produce items suitable to place on proof-action list."
   :type 'function
   :group 'proof-server)

(defcustom proof-everything-sent-fun nil
  "Predicate tests whether everything has been sent to prover."
  :type 'function
  :group 'proof-server)

(defcustom proof-server-log-traffic nil
  "Log traffic between emacs and prover to a buffer."
  :type 'boolean
  :group 'proof-server)

(defcustom proof-server-handle-output-system-specific nil
  "Set this variable to handle system specific output.
Errors and interrupts are recognised in the function
`proof-server-handle-immediate-output'.  Later output is
handled by `proof-server-handle-delayed-output', which
displays messages to the user in *goals* and *response*
buffers.

This hook can run between the two stages to take some effect.

It should be a function which is passed (cmd string) as
arguments, where `cmd' is a string containing the currently
processed command and `string' is the response from the proof
system.  If action is taken and goals/response display should
be prevented, the function should update the variable
`proof-prover-last-output-kind' to some non-nil symbol.

The symbol will be compared against standard ones, see documentation
of `proof-prover-last-output-kind'.  A suggested canonical non-standard
symbol is 'systemspecific."
  :type '(repeat function)
  :group 'proof-server)

(defcustom proof-server-interruptp-fun nil
   "Predicate that returns true if a prover response is an interrupt."
   :type '(choice function (const nil))
   :group 'proof-server)

(defcustom proof-server-errorp-fun nil
   "Predicate that returns true if a prover response indicates an error."
   :type '(choice function (const nil))
   :group 'proof-server)

(defcustom proof-server-proof-completedp-fun nil
   "Predicate that returns true if a prover response indicates a proof has been completed."
   :type '(choice function (const nil))
   :group 'proof-server)

(defcustom proof-server-start-silent-cmd nil
  "Command to turn prover goals output off when sending many script commands.
If non-nil, Proof General will automatically issue this command
to help speed up processing of long proof scripts.
See also `proof-server-stop-silent-cmd'."
  :type '(choice string (const nil))
  :group 'proof-server)

(defcustom proof-server-stop-silent-cmd nil
  "Command to turn prover output on.
If non-nil, Proof General will automatically issue this command
to help speed up processing of long proof scripts.
See also `proof-server-start-silent-cmd'."
  :type '(choice string (const nil))
  :group 'proof-server)

(defcustom proof-server-silent-threshold 2
  "Number of waiting commands in the proof queue needed to trigger silent mode.
Default is 2, but you can raise this in case switching silent mode
on or off is particularly expensive (or make it ridiculously large
to disable silent mode altogether)."
  :type 'integer
  :group 'proof-server)

(defcustom proof-script-preprocess nil
  "Function to pre-process (SPAN STRING) taken from proof script."
  :type 'function
  :group 'proof-server)
  
(defcustom proof-tokens-activate-command nil
  "Command to activate token input/output for prover.
If non-nil, this command is sent to the proof assistant when
Unicode Tokens support is activated."
  :type 'string
  :group 'proof-server)

(defcustom proof-tokens-deactivate-command nil
  "Command to deactivate token input/output for prover.
If non-nil, this command is sent to the proof assistant when
Unicode Tokens support is deactivated."
  :type 'string
  :group 'proof-server)

(defcustom proof-tokens-extra-modes nil
  "List of additional mode names to use with Proof General tokens.
These modes will have Tokens enabled for the proof assistant token language,
in addition to the four modes for Proof General (script, shell, response, pbp).

Set this variable if you want additional modes to also display
tokens (for example, editing documentation or source code files)."
  :type '(repeat symbol)
  :group 'proof-server)

(defcustom proof-server-filename-escapes nil
  "A list of escapes that are applied to %s for filenames.
A list of cons cells, car of which is string to be replaced
by the cdr.
For example, when directories are sent to Isabelle, HOL, and Coq,
they appear inside ML strings and the backslash character and
quote characters must be escaped.  The setting
  '((\"\\\\\\\\\" . \"\\\\\\\\\")
    (\"\\\"\" . \"\\\\\\\"\"))
achieves this.   This does not apply to LEGO, which does not
need backslash escapes and does not allow filenames with
quote characters.

This setting is used inside the function `proof-format-filename'."
  :type '(list (cons string string))
  :group 'proof-server)

(defcustom proof-assert-command-hook nil
  "Hooks run before asserting a command (or a set of commands).
Can be used to insert commands before any (set of) input sent
by the user. It is run by `proof-assert-until-point'.

WARNING: don't call `proof-assert-until-point' in this hook, you
would loop forever.

Example of use: Insert a command to adapt printing width. Note
that `proof-shell-insert-hook' may be use instead (see lego mode)
if no more prompt will be displayed (see
`proof-shell-insert-hook' for details)."
  :type '(repeat function)
  :group 'proof-server)

(defcustom proof-retract-command-hook nil
  "Hooks run before retracting a command (or a set of commands).
Can be used to insert commands. It is run by
`proof-retract-until-point'.

WARNING: don't call `proof-retract-until-point' in this hook, you
would loop forever.

Example of use: Insert a command to adapt printing width. Note
that `proof-shell-insert-hook' may be use instead (see lego mode)
if no more prompt will be displayed (see
`proof-shell-insert-hook' for details)."
  :type '(repeat function)
  :group 'proof-server)

(defcustom proof-server-handle-delayed-output-hook
  '(proof-pbp-focus-on-first-goal)
  "Hooks run after new output has been displayed in goals or response buffer."
  :type '(repeat function)
  :group 'proof-server)

(defcustom proof-server-handle-error-or-interrupt-hook
  '(proof-goto-end-of-locked-on-error-if-pos-not-visible-in-window)
  "Run after an error or interrupt has been reported in the response buffer.
Hook functions may inspect `proof-shell-last-output-kind' to
determine whether the cause was an error or interrupt.  Possible
values for this hook include:

 `proof-goto-end-of-locked-on-error-if-pos-not-visible-in-window'
 `proof-goto-end-of-locked-if-pos-not-visible-in-window'

which move the cursor in the scripting buffer on an error or
error/interrupt.

Remark: This hook is called from shell buffer.  If you want to do
something in scripting buffer, `save-excursion' and/or `set-buffer'."
  :type '(repeat function)
  :group 'proof-server)

'(defcustom proof-server-signal-interrupt-hook nil
  "Run when the user tries to interrupt the prover.
This hook is run inside `proof-interrupt-process' when the user
tries to interrupt the proof process. It is therefore run earlier
than `proof-shell-handle-error-or-interrupt-hook', which runs
when the interrupt is acknowledged inside `proof-shell-exec-loop'.

This hook also runs when the proof assistent is killed.

Hook functions should set the dynamic variable `prover-was-busy'
to t if there might have been a reason to interrupt. Otherwise
the generic interrupt handler might issue a prover-not-busy
error."
  :type '(repeat function)
  :group 'proof-server)

(defcustom proof-state-change-hook nil
  "This hook is called when a scripting state change may have occurred.
Specifically, this hook is called after a region has been asserted or
retracted, or after a command has been sent to the prover with
`proof-server-invisible-command'.

This hook is used within Proof General to refresh the toolbar."
  :type '(repeat function)
  :group 'proof-server)


;;
;; 6. Goals buffer
;;

(defgroup proof-goals nil
  "Settings for configuring the goals buffer."
  :group 'prover-config
  :prefix "pg-goals-")

(defcustom pg-subterm-first-special-char nil
  "First special character.
Codes above this character can have special meaning to Proof General,
and are stripped from the prover's output strings.
Leave unset if no special characters are being used."
  :type '(choice character (const nil))
  :group 'proof-goals)

(defcustom pg-subterm-anns-use-stack nil
  "Choice of syntax tree encoding for terms.

If nil, prover is expected to make no optimisations.
If non-nil, the pretty printer of the prover only reports local changes.
For LEGO 1.3.1 use nil, for Coq 6.2, use t."
  :type 'boolean
  :group 'proof-goals)

(defcustom pg-goals-change-goal nil
  "Command to change to the goal `%s'."
  :type 'string
  :group 'proof-goals)

(defcustom pbp-goal-command nil
  "Command sent when `pg-goals-button-action' is requested on a goal."
  :type '(choice (const nil) string)
  :group 'proof-goals)

(defcustom pbp-hyp-command nil
  "Command sent when `pg-goals-button-action' is requested on an assumption."
  :type '(choice (const nil) string)
  :group 'proof-goals)

(defcustom pg-subterm-help-cmd nil
  "Command to display mouse help about a subterm.
This command is sent to the proof assistant, replacing %s by the
subterm that the mouse is over."
  :type '(choice (const nil) string)
  :group 'proof-goals)

(defcustom pg-goals-error-regexp nil
  "Regexp indicating that the proof process has identified an error."
  :type '(choice (const nil) regexp)
  :group 'proof-goals)

(defcustom proof-shell-result-start nil
  "Regexp matching start of an output from the prover after pbp commands.
In particular, after a `pbp-goal-command' or a `pbp-hyp-command'."
  :type '(choice (const nil) regexp)
  :group 'proof-goals)

(defcustom proof-shell-result-end ""
  "Regexp matching end of output from the prover after pbp commands.
In particular, after a `pbp-goal-command' or a `pbp-hyp-command'."
  :type 'regexp
  :group 'proof-goals)

(defcustom pg-subterm-start-char nil
  "Opening special character for subterm markup.
Subsequent special characters with values *below*
`pg-subterm-first-special-char' are assumed to be subterm position
indicators.  Annotations should be finished with `pg-subterm-sep-char';
the end of the concrete syntax is indicated by `pg-subterm-end-char'.

If `pg-subterm-start-char' is nil, subterm markup is disabled."
  :type '(choice character (const nil))
  :group 'proof-goals)

(defcustom pg-subterm-sep-char nil
  "Finishing special for a subterm markup.
See doc of `pg-subterm-start-char'."
  :type '(choice character (const nil))
  :group 'proof-goals)

(defcustom pg-subterm-end-char nil
  "Closing special character for subterm markup.
See `pg-subterm-start-char'."
  :type 'character
  :group 'proof-goals)

(defcustom pg-topterm-regexp nil
  "Annotation regexp that indicates the beginning of a \"top\" element.
A \"top\" element may be a sub-goal to be proved or a named hypothesis,
for example.  It could also be a literal command to insert and
send back to the prover.

The function `pg-topterm-goalhyplit-fn' examines text following this
special character, to determine what kind of top element it is.

This setting is also used to see if proof-by-pointing features
are configured.  If it is unset, some of the code
for parsing the prover output is disabled."
  :type 'character
  :group 'proof-goals)

(defcustom proof-goals-font-lock-keywords nil
  "Value of `font-lock-keywords' used to fontify the goals output.
The goals shell mode should set this before calling `proof-goals-config-done'.
See also `proof-script-font-lock-keywords' and `proof-response-font-lock-keywords'."
  :type 'sexp
  :group 'proof-goals)

(defcustom proof-response-font-lock-keywords nil
  "Value of `font-lock-keywords' used to fontify the response output.
The response mode should set this before calling `proof-response-config-done'.
See also `proof-script-font-lock-keywords' and `proof-goals-font-lock-keywords'."
  :type 'sexp
  :group 'proof-goals)

(defcustom proof-shell-font-lock-keywords nil
  "Value of `font-lock-keywords' used to fontify the shell buiffer.
The shell mode should may this before calling `proof-response-config-done'.
Note that by default, font lock is turned *off* in shell buffers to
improve performance.  If you need to understand some output it may help
to turn it on temporarily.
See also `proof-script-font-lock-keywords', `proof-goals-font-lock-keywords'
and `proof-response-font-lock-keywords'."
  :type 'sexp
  :group 'proof-goals)

(defcustom pg-before-fontify-output-hook nil
  "This hook is called before fontifying a region in an output buffer.
A function on this hook can alter the region of the buffer within
the current restriction, and must return the final value of (point-max).
\[This hook is presently only used by phox-sym-lock]."
  :type '(repeat function)
  :group 'proof-goals)

(defcustom pg-after-fontify-output-hook nil
  "This hook is called before fonfitying a region in an output buffer.
\[This hook is presently only used by Isabelle]."
  :type '(repeat function)
  :group 'proof-goals)


(provide 'proof-config)
;;; proof-config.el ends here
