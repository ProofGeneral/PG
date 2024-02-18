;;; proof-tree.el --- Proof General prooftree communication.  -*- lexical-binding: t; -*-

;; This file is part of Proof General.

;; Portions © Copyright 1994-2012  David Aspinall and University of Edinburgh
;; Portions © Copyright 2003-2021  Free Software Foundation, Inc.
;; Portions © Copyright 2001-2017  Pierre Courtieu
;; Portions © Copyright 2010, 2016  Erik Martin-Dorel
;; Portions © Copyright 2011-2013, 2016-2017, 2019-2021  Hendrik Tews
;; Portions © Copyright 2015-2017  Clément Pit-Claudel

;; Authors:   Hendrik Tews

;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:
;;
;; Generic code for the communication with prooftree.  Prooftree
;; is an ocaml-gtk program that displays proof trees.
;;
;; The main problem with proof tree visualization is that Coq (and
;; probably other proof assistants as well) does not provide any
;; information about which subgoals are new and have been created by
;; the last proof command and which subgoals stem from older proof
;; commands.
;;
;; To solve this problem prooftree relies on unique identification
;; strings of goals, which are called goal or sequent ID's in the
;; code.  With these ID's it is easy to keep track which goals are new.
;;
;; A second problem is that, for an undo command, Coq (and probably
;; other proof assistants as well) does not tell which subgoals and
;; which finished branches must be deleted.  Therefore prooftree needs
;; a continuously increasing proof state number and keeps a complete
;; undo history for every proof.
;;
;; A third problem is that Coq (and probably other proof assistants as
;; well) is not able to generate the information for a proof tree in
;; the middle of a proof.  Therefore, if the user wants to start the
;; proof-tree display in the middle of the proof, it is necessary to
;; retract to the start of the proof and then to reassert to the
;; previous end of the locked region.  To achieve this, one has to call
;; `accept-process-output' at suitable points to let Proof General
;; process the `proof-action-list'.
;; 
;; A fourth problem is that proof-tree display can only work when the
;; prover output is not suppressed (via `proof-full-annotation').
;; `proof-shell-should-be-silent' takes care of that.
;; 
;; Earlier versions of prooftree maintained certain data structures
;; twice, one time in prooftree and one time in Proof General. The
;; copy in Proof General was necessary, because all necessary show
;; goal commands had to be generated and processed immediately in the
;; current state.
;;
;; With Coq 8.10 it is possible to issue a show goal command for an
;; earlier state, therefore, in the current version all the processing
;; is done in prooftree and prooftree sends asynchronous show goal
;; requests to Proof General, if sequents are missing or must be
;; updated. These show goal requests are processed in Proof General as
;; priority actions, nevertheless, they can be delayed for quite some
;; time. It might even happen, that the first show goal command for an
;; additionally spawned subgoal arrives at Proof General only after
;; the proof has been finished. Prooftree can request additinal
;; sequent updates for instantiated existentials only after that first
;; sequent has arrived. Prooftree keeps a tree with the instantiation
;; dependencies of existentials, such that it can immediately issue
;; all sequent update requests for all known instantiations. With this
;; there is at most one round trip to prooftree necessary after a
;; proof has been finished in Proof General.
;;
;; In the proof assistant source code, the next lemma might start
;; immediately after finishing the preceeding proof. Before processing
;; that next command, the proof tree display must be switched off in
;; Proof General and for Coq, the command for disabling the dependent
;; evar line must be processed. On the other hand, all show goal
;; commands arriving late for the preceeding proof must be completely
;; processed with proof tree display enabled. To solve that problem,
;; `proof-tree-check-proof-finish' is called as urgent action inside
;; `proof-shell-exec-loop', before the next command in the queue
;; region is sent to the proof assistant. If this urgent action
;; recognizes a proof end, it moves all non-priority actions from
;; `proof-action-list' to `proof-tree--delayed-actions' signaling
;; `proof-second-action-list-active'. This effectively stops
;; processing of the queue region. When the end of the proof is
;; processed in the delayed output handling, Proof General sends a
;; proof-complete message to prooftree, thereby requesting a
;; confirm-proof-complete from prooftree after prooftree has sent all
;; show goal requests to Proof General. Via
;; `proof-tree-display-stop-command' the confirmation message causes
;; the insertion of an action item with tag 'proof-tree-last-item.
;; When this item arrives inside `proof-tree-check-proof-finish', all
;; show goal commands have been processed, because care is taken to
;; process priority actions in order. `proof-tree-check-proof-finish'
;; can therefore move the queued commands back to `proof-action-list'
;; and continue normal processing.
;;
;; Apart from the urgent action discussed before, 
;; the glue code here works on the delayed output.  That is,
;; the glue code here runs when the next proof command has already
;; been sent to the proof assistant.  The glue code makes a light
;; analysis on the output of the proof assistant, extracts the
;; necessary parts with regular expressions and sends appropriate
;; commands to prooftree.  This is achieved by calling the main entry
;; here, the function `proof-tree-handle-delayed-output' from the
;; proof shell filter function after `proof-shell-exec-loop' has
;; finished.
;;
;; Actually, for every proof, Prooftree can display a set of disjunct
;; proof trees, which are organized into layers.  More than one proof
;; tree in more than one layer is needed to support the Grap
;; Existential Variables command in Coq.  There is one proof tree in
;; the first layer for the original goal.  The second layer contains
;; all the goals that the first Grab Existential Variables command
;; created from uninstantiated existential variabes in the main proof.
;; The third layer contains the goals that the second Grap Existential
;; Variables created.

;;; Code:

(eval-when-compile (require 'cl-lib))   ;cl-assert
(eval-when-compile
  (require 'proof-shell))


;;
;; User options
;;

(defgroup proof-tree ()
  "Customization for the proof tree visualizer"
  :group 'proof-general
  :package-version '(ProofGeneral . "4.2"))

(defcustom proof-tree-program (proof-locate-executable "prooftree" t nil)
  "Command to invoke prooftree."
  :type 'string
  :group 'proof-tree)

(defcustom proof-tree-arguments ()
  "Command line arguments for prooftree."
  :type '(repeat string)
  :group 'proof-tree)


;;
;; Proof assistant options
;;

(defgroup proof-tree-internals ()
  "Proof assistant specific customization of prooftree."
  :group 'proof-general-internals
  :package-version '(ProofGeneral . "4.2"))

;; defcustom proof-tree-configured is in proof-config.el, because it is
;; needed in pg-custom.el

(defcustom proof-tree-ignored-commands-regexp nil
  "Commands that should be ignored for the prooftree display.
The output of commands matching this regular expression is not
sent to prooftree.  It should match commands that display
additional information but do not make any proof progress.  Leave
at nil to act on all commands.

For Coq this regular expression should contain all commands such
as Lemma, that can start a proof."
  :type '(choice regexp (const nil))
  :group 'proof-tree-internals)

(defcustom proof-tree-navigation-command-regexp nil
  "Regexp to match a navigation command.
A navigation command typically focusses on a different open goal
without changing any of the open goals.  Leave at nil if there are
no navigation commands."
  :type '(choice regexp (const nil))
  :group 'proof-tree-internals)

(defcustom proof-tree-cheating-regexp nil
  "Regexp to match cheating proofer commands.
A cheating command finishes the current goal without proving it
to permit the user to first focus on other parts of the
development.  Examples are \"sorry\" in Isabelle and \"admit\" in
Coq.  Leave at nil if there are no cheating commands."
  :type '(choice regexp (const nil))
  :group 'proof-tree-internals)

(defcustom proof-tree-current-goal-regexp nil
  "Regexp to match the current goal and its ID.
The regexp is matched against the output of the proof assistant
to extract the current goal with its ID.  The regexp must have 2
grouping constructs, the first one matches just the ID, the
second one the complete sequent text that is to be sent to
prooftree."
  :type 'regexp
  :group 'proof-tree-internals)

(defcustom proof-tree-update-goal-regexp nil
  "Regexp to match a goal and its ID and the corresponding state.
The regexp is matched against the output of additional show-goal
commands inserted by prooftree with a command returned by
`proof-tree-show-sequent-command'.  Prooftree inserts such
commands to update the goal texts.  This is necessary,
for instance, when existential variables get instantiated.  This
regexp must have 3 grouping constructs, the first matching the ID
of the goal, the second the proof assistant state to which the
sequent text corresponds, which must be a number, and the third
the complete sequent text."
  :type 'regexp
  :group 'proof-tree-internals)

(defcustom proof-tree-additional-subgoal-ID-regexp nil
  "Regular expression to match ID's of additional subgoals.
This regexp is used to extract the ID's of all additionally open
goals.  The regexp is used in a while loop and must match one
subgoal ID with subgroup 1."
  :type 'regexp
  :group 'proof-tree-internals)

(defcustom proof-tree-existentials-state-start-regexp nil
  "Regexp to match the start of the state display of existential variables.
Together with `proof-tree-existentials-state-end-regexp', this
regular expression is used to determine the state display of
existential variables, which contains information about which
existentials are still uninstantiated and about dependencies of
instantiated existential variables.  Leave this variable nil, if
there is no such state display."
  :type '(choice regexp (const nil))
  :group 'proof-tree-internals)

(defcustom proof-tree-existentials-state-end-regexp nil
  "Regexp to match the end of the state display of existential variables.
Together with `proof-tree-existentials-state-start-regexp', this
regular expression is used to determine the state display of
existential variables, which contains information about which
existentials are still uninstantiated and about dependencies of
instantiated existential variables.  If this variable is nil (and
if `proof-tree-existentials-state-start-regexp' is non-nil), then
the state display expands to the end of the prover output."
  :type '(choice regexp (const nil))
  :group 'proof-tree-internals)

(defcustom proof-tree-branch-finished-regexp nil
  "Regexp to recognize that the current branch has been finished.
This must match in precisely the following cases:
- The current branch has been finished but there is no current
  open subgoal because the prover has not switched to the next
  subgoal.
- The last open goal has been proved."
  :type 'regexp
  :group 'proof-tree-internals)

(defcustom proof-tree-get-proof-info nil
  "Proof assistant specific function for information about the current proof.
This function takes no arguments.  It must return a list, which
contains, in the following order:

* the current state number (as positive integer)
* the name of the current proof (as string) or nil

The state number is used to implement undo in prooftree.  The
proof name is used to distinguish different proofs inside
prooftree.

The state number is interpreted as the state that has been
reached after the last command has been processed.  It must be
consistent in the following sense.  Firstly, it must be strictly
increasing for successive commands that can be individually
retracted.  Secondly, the state number reported after some command
X has been processed must be strictly greater than the state
reported when X is retracted.  Finally, state numbers of commands
preceding X must be less than or equal the state reported when X
is retracted (but no stuff before X)."
  :type 'function
  :group 'proof-tree-internals)

(defcustom proof-tree-show-sequent-command nil
  "Proof assistant specific function for a show-goal command.
This function is applied to a goal ID and a state number and
should return a command (as string) that will display the
complete sequent with that ID in the given state. The regexp
`proof-tree-update-goal-regexp' should match the output of the
proof assistant for the returned command, such that
`proof-tree-update-sequent' will update the sequent ID inside
prooftree.

If the proof assistant is unable to redisplay sequent ID the
function should return nil and prooftree will not get updated."
  :type 'function
  :group 'proof-tree-internals)

(defcustom proof-tree-find-begin-of-unfinished-proof nil
  "Proof assistant specific function for the start of the current proof.
This function is called with no argument when the user switches
the external proof-tree display on.  Then, this function must
determine if there is a currently unfinished proof for which the
proof-tree display should be started.  If yes this function must
return the starting position of the command that started this
proof.  If there is no such proof, this function must return nil."
  :type 'function
  :group 'proof-tree-internals)

(defcustom proof-tree-find-undo-position nil
  "Proof assistant specific function for finding the point to undo to.
This function is used to convert the state number, which comes
with an undo command from Prooftree, into a point position for
`proof-retract-until-point'.  This function is called in the
current scripting buffer with the state number as argument.  It
must return a buffer position."
  :type 'function
  :group 'proof-tree-internals)

(defcustom proof-tree-start-display-hook ()
  "Normal hook for prooftree when external display starts.
This hook is called when the external display is startet, more
precisely, when the proof assistant is in the state that Proof
General starts to send display commands to prooftree. This means,
retraction to the start of the proof, in case it was necessary,
has been done and `proof-action-list` is empty.

This hook is used, for instance, for Coq to enable the dependent
evars line."
  :type 'hook
  :group 'proof-tree-internals)

(defcustom proof-tree-display-stop-command ()
  "Function for the last command before switching off proof-tree display.
This is a proof assistant specific function that must be
instantiated. When a proof finishes with proof-tree display,
prooftree may request a number of show goal commands after the
proof has been finished in the proof assistant. This function
must return an action item that can be inserted as last command
in `proof-action-list' after all these show goal commands.

For Coq this is used to disable the dependent evar line. But also
other proof assistants that enable the proof tree display must
set this function.

An action item is a list `(SPAN COMMANDS ACTION [DISPLAYFLAGS])',
see `proof-action-list'. The action item must not be recognized
as comment by `proof-shell-slurp-comments', that is COMMANDS must
be a nonempty list of strings. The generic prooftree glue code
will add 'proof-tree-last-item to DISPLAYFLAGS."
  :type 'function
  :group 'proof-tree-internals)

;;
;; Internal variables
;;

(defvar proof-tree-external-display nil
  "Display proof trees in external prooftree windows if t.
Actually, if this variable is t then the user requested an
external proof-tree display.  If there was no unfinished proof
when proof-tree display was requested and if no proof has been
started since then, then there is obviously no proof-tree
display.  In this case, this variable stays t and the proof-tree
display will be started for the next proof.

Controlled by `proof-tree-external-display-toggle'.")

(defvar proof-tree-process nil
  "Emacs Lisp process object of the prooftree process.")

(defconst proof-tree-process-name "proof-tree"
  "Name of the prooftree process for Emacs Lisp.")

(defconst proof-tree-process-buffer-name
  (concat "*" proof-tree-process-name "*")
  "Name of the buffer for stdout and stderr of the prooftree process.")

(defvar proof-tree-process-buffer nil
  "Buffer for stdout and stderr of the prooftree process.")

(defconst proof-tree-emacs-exec-regexp
  "\nemacs exec: \\([-a-z]+\\) *\\([^\n]*\\)\n"
  "Regular expression to match callback commands from the prooftree process.")

(defconst proof-tree--show-goal-command-regexp
  "\"\\([^\"]*\\)\" at \\([0-9]*\\) for \"\\([^\"]*\\)\""
  "Regular expression to match the data of the prooftree show-goal command.")

(defconst proof-tree--confirm-complete-regexp
  "\"\\([^\"]*\\)\""
  "Regular expression to match the data of the confirm-proof-complete command.")

(defvar proof-tree-last-state 0
  "Last state of the proof assistant.
Used for undoing in prooftree.")

(defvar proof-tree-current-proof nil
  "Name of the current proof or nil if there is none.
This variable is only maintained and meaningful if
`proof-tree-external-display' is t.")

(defvar proof-tree--delayed-actions nil
  "Hold delayed action items when waiting for prooftree after proof end.
This internal variable is completely managed by
`proof-tree-check-proof-finish'. After a proof with proof-tree
display has been finished, it holds the following commands, until
all requested show goal commands have been processed. If this
variable is non-nil, then `proof-second-action-list-active' must
be set. In addition to holding the to be delayed action items,
this variable is used to remember that
`proof-tree-check-proof-finish' waits for
`proof-tree-display-stop-command' after it recognized the end of
a proof. Therefore, if non-nil, this variable must either hold a
nonempty list (the to be delayed action items) or t (if there are
no action items to be delayed.")


;;; Missing in the library

(defun list-pred-partition (p l)
  "Partition list L according to P.
Returns a cons pair `(L1 . L2)' of lists, where L1 contains the
elements of L for which P returns non-nil and L2 those for which
P returns nil. The concatenation of L1 and L2 is a permutation of
L, the order of elements in L is preserved. P must be a function
that takes one argument."
  (let (yes no)
    (dolist (x l (cons (nreverse yes) (nreverse no)))
      (if (funcall p x)
          (push x yes)
        (push x no)))))
  
;;
;; process filter function that receives prooftree output
;;

(defvar proof-tree-output-marker nil
  "Marker in `proof-tree-process-buffer' pointing to new output.
This marker points to the next piece of output that needs to get processed.")

(defvar proof-tree-filter-continuation nil
  "Continuation when `proof-tree-process-filter' stops early.
A function that handles a command from Prooftee might fail
because not all data from Prooftee has yet arrived.  In this case
the continuation is stored in this variable and will be called
from `proof-tree-process-filter' when more output arrives.")


(defun proof-tree-handle-goal-request (data)
  "Handle a show-goal command from prooftree.
The command from prooftree has the form \"emacs exec: show-goal
\"29\" at 21\"."
  (if (string-match proof-tree--show-goal-command-regexp data)
      (let ((goal-id (match-string 1 data))
            (state (string-to-number (match-string 2 data)))
            (proof-name (match-string 3 data))
            show-cmd)
        ;; (message "show goal %s at %d for %s" goal-id state proof-name)
        (setq show-cmd (funcall proof-tree-show-sequent-command goal-id state))
	(when show-cmd
          (proof-add-to-priority-queue
	   (proof-shell-action-list-item
	    show-cmd
	    (proof-tree-make-show-goal-callback proof-name)
	    '(no-goals-display no-response-display proof-tree-show-subgoal)))))
    (display-warning
     '(proof-general proof-tree)
     (format "Malformed prooftree show-goal command") :warning)))

(defun proof-tree-stop-external-display ()
  "Prooftree callback for the command \"stop-displaying\"."
  (if proof-tree-current-proof
      (message "External proof-tree display switched off"))
  (proof-tree-quit-proof)
  (setq proof-tree-external-display nil))

(defun proof-tree-handle-proof-tree-undo (data)
  "Handle an undo command that arrives from prooftree."
  (let ((undo-state (string-to-number data)))
    (if (and (integerp undo-state) (> undo-state 0))
	(with-current-buffer proof-script-buffer
	  (goto-char (funcall proof-tree-find-undo-position undo-state))
	  (proof-retract-until-point))
      (display-warning
       '(proof-general proof-tree)
       "Prooftree sent an invalid state for undo"
       :warning))))

(defun proof-tree-insert-script (data)
  "Handle an insert-command command from Prooftree."
  (let ((command-length (string-to-number data)))
    (if (and (integerp command-length) (> command-length 0))
	(condition-case nil
	    (progn
	      (insert
	       (with-current-buffer proof-tree-process-buffer
		 (buffer-substring
		  proof-tree-output-marker
		  (+ proof-tree-output-marker command-length))))
	      ;; received all text -> advance marker
	      (set-marker proof-tree-output-marker
			  (+ proof-tree-output-marker command-length)
			  proof-tree-process-buffer))
	  (args-out-of-range
	   ;; buffer substring failed because the end position is not
	   ;; there yet
	   ;; need to try again later
	   (setq proof-tree-filter-continuation
		 (lambda () (proof-tree-insert-script data)))))
      (display-warning
       '(proof-general proof-tree)
       "Prooftree sent an invalid data length for insert-command"
       :warning))))

(defun proof-tree-confirm-proof-complete (data)
  "Callback function for confirm-proof-complete messages.
Add command `proof-tree-display-stop-command' with
'proof-tree-last-item flag, such that
`proof-tree-check-proof-finish' eventually sees this last command
and switches the proof-tree display processing off."
  (if (string-match proof-tree--confirm-complete-regexp data)
      (let ((stop-cmd (funcall proof-tree-display-stop-command)))
        ;; an action list item is a list (span commands action [displayflags])
        (proof-add-to-priority-queue
         (list (car stop-cmd) (nth 1 stop-cmd) (nth 2 stop-cmd)
               (cons 'proof-tree-last-item (nth 3 stop-cmd)))))
    (display-warning
     '(proof-general proof-tree)
     "Malformed prooftree confirm-proof-complete command" :error)))

(defun proof-tree-insert-output (string &optional message)
  "Insert output or a message into the prooftree process buffer.
If MESSAGE is t, a message is inserted and
`proof-tree-output-marker' is not touched. Otherwise, if
`proof-tree-output-marker' is nil, it is set to point to the
newly arrived output."
  (with-current-buffer proof-tree-process-buffer
    (let ((moving (= (point) (point-max))))
      (save-excursion
	(when (and (not message) (not proof-tree-output-marker))
	  (setq proof-tree-output-marker (point-max-marker))
	  (set-marker-insertion-type proof-tree-output-marker nil))
	(goto-char (point-max))
	(insert string))
      (if moving (goto-char (point-max))))))


(defun proof-tree-process-filter-internal (string)
  "Output filter for prooftree - internal worker part.
This function is the worker for `proof-tree-process-filter', it
implements all the functionality of `proof-tree-process-filter'.

Records the output in the prooftree process buffer and checks for
callback function requests. Such callback functions might fail
because the complete output from Prooftree has not arrived yet.
In this case they store a continuation function in
`proof-tree-filter-continuation that will be called when the next
piece of output arrives. `proof-tree-output-marker' points to the
next piece of Prooftree output that needs to get processed. If
everything is processed, the marker is deleted and
`proof-tree-insert-output' sets it again for the next output.

This function relies on the POSIX guarantee that up to 512 bytes
are transmitted atomically over a pipe."
  ;; (message "PTP filter received |%s|" string)
  (proof-tree-insert-output string)
  (let ((continuation proof-tree-filter-continuation)
	command-found command data)
    ;; clear continuation
    (setq proof-tree-filter-continuation nil)
    ;; call continuation -- this might set a continuation again
    (when continuation
      (funcall continuation))
    (unless proof-tree-filter-continuation
      ;; there was no continuation or the continuation finished successfully
      ;; need to look for command after output marker
      (while (< proof-tree-output-marker
		(1+ (buffer-size proof-tree-process-buffer)))
	;; there is something after the output marker
	(with-current-buffer proof-tree-process-buffer
	  (save-excursion
	    (goto-char proof-tree-output-marker)
	    (setq command-found
		  (proof-re-search-forward proof-tree-emacs-exec-regexp nil t))
	    (if command-found
		(progn
		  (setq command (match-string 1)
			data (match-string 2))
		  (set-marker proof-tree-output-marker (point)))
	      (set-marker proof-tree-output-marker (point-max)))))
	(when command-found
	  (cond
           ((equal command "show-goal")
            (proof-tree-handle-goal-request data))
	   ((equal command "stop-displaying")
	    (proof-tree-stop-external-display))
	   ((equal command "undo")
	    (proof-tree-handle-proof-tree-undo data))
	   ((equal command "insert-proof-script")
	    (proof-tree-insert-script data))
           ((equal command "confirm-proof-complete")
            (proof-tree-confirm-proof-complete data))
	   (t
	    (display-warning
	     '(proof-general proof-tree)
	     (format "Unknown prooftree command %s" command)
	     :error))))))
    ;; one of the handling functions might have set a continuation
    ;; if not we clear the output marker
    (unless proof-tree-filter-continuation
      (set-marker proof-tree-output-marker nil)
      (setq proof-tree-output-marker nil))))

(defun proof-tree-process-filter (_proc string)
  "Output filter for prooftree.
This function is the exception wrapper, all the functionality is
implemented in `proof-tree-process-filter-internal'. The filter
records the output in `proof-tree-process-buffer' and calls
callback functions as necessary."
  (condition-case err
      (proof-tree-process-filter-internal string)
    (error
     (message "Error escaping proof-tree-process-filter: %s" err)
     (signal (car err) (cdr err)))))
;;
;; Process creation
;;

(defun proof-tree-process-sentinel (_proc event)
  "Sentinel for prooftee.
Runs on process status changes and cleans up when prooftree dies."
  (proof-tree-insert-output (concat "\nsubprocess status change: " event) t)
  (unless (proof-tree-is-running)
    (proof-tree-stop-external-display)
    (setq proof-tree-process nil)))

(defun proof-tree-start-process ()
  "Start the external prooftree process.
Does also initialize the communication channel and some internal
variables."
  (let ((process-connection-type nil)	; use pipes, see emacs bug #24531
	(old-proof-tree (get-process proof-tree-process-name)))
    ;; reset output marker
    (when proof-tree-output-marker
      (set-marker proof-tree-output-marker nil)
      (setq proof-tree-output-marker nil))
    ;; create buffer
    (setq proof-tree-process-buffer
	  (get-buffer-create proof-tree-process-buffer-name))
    ;; first clean up any old processes
    (when old-proof-tree
      (delete-process old-proof-tree)
      (proof-tree-insert-output
       "\n\nProcess terminated by Proof General\n\n" t))
    ;; now start the new process
    (proof-tree-insert-output "\nStart new prooftree process\n\n" t)
    (setq proof-tree-process
	  (apply #'start-process
	   proof-tree-process-name
	   proof-tree-process-buffer
	   proof-tree-program
	   proof-tree-arguments))
    (set-process-coding-system proof-tree-process 'utf-8-unix 'utf-8-unix)
    (set-process-filter proof-tree-process #'proof-tree-process-filter)
    (set-process-sentinel proof-tree-process #'proof-tree-process-sentinel)
    (set-process-query-on-exit-flag proof-tree-process nil)
    ;; other initializations
    (proof-tree-send-configure)))


(defun proof-tree-is-running ()
  "Return t if prooftree is properly running."
  (and proof-tree-process
       (eq (process-status proof-tree-process) 'run)))

(defun proof-tree-ensure-running ()
  "Ensure the prooftree process is running properly."
  (unless (proof-tree-is-running)
    (proof-tree-start-process)))


;;
;; Low-level communication primitives
;;

(defconst proof-tree-protocol-version 4
  "Version of the communication protocol between Proof General and Prooftree.
Must be increased if one of the low-level communication
primitives is changed.")

(defun proof-tree-send-message (second-line data)
  "Send a complete message to Prooftree.
Send a message with command line SECOND-LINE and all strings in
DATA as data sections to Prooftree."
  (let ((second-line-len (string-bytes second-line)))
    (cl-assert (< second-line-len 999))
    (process-send-string
     proof-tree-process
     (format "second line %03d\n%s\n%s%s"
	     (1+ second-line-len)
	     second-line
	     (mapconcat #'identity data "\n")
	     (if data "\n" "")))))

(defun proof-tree-send-configure ()
  "Send the configure message."
  (proof-tree-send-message
   (format "configure for \"%s\" and protocol version %02d"
	   proof-assistant
	   proof-tree-protocol-version)
   ()))

(defun proof-tree-send-goal-state (state proof-name command-string cheated-flag
				   current-sequent-id
				   current-sequent-text additional-sequent-ids
				   existential-info)
  "Send the current goal state to prooftree."
  ;; (message "PTSGS id %s command %s sequent %s ex-info %s"
  ;; 	   current-sequent-id command-string current-sequent-text
  ;;          existential-info)
  (let* ((add-id-string (mapconcat #'identity additional-sequent-ids " "))
	 (second-line
	  (format
	   (concat "current-goals state %d current-sequent %s %s "
		   "proof-name-bytes %d "
		   "command-bytes %d sequent-text-bytes %d "
		   "additional-id-bytes %d existential-bytes %d")
	   state
	   current-sequent-id
	   (if cheated-flag "cheated" "not-cheated")
	   (1+ (string-bytes proof-name))
	   (1+ (string-bytes command-string))
	   (1+ (string-bytes current-sequent-text))
	   (1+ (string-bytes add-id-string))
	   (1+ (string-bytes existential-info)))))
    (proof-tree-send-message
     second-line
     (list proof-name command-string current-sequent-text
	   add-id-string existential-info))))

(defun proof-tree-send-update-sequent (state proof-name sequent-id sequent-text
                                       existential-info)
  "Send the updated sequent text to prooftree."
  ;; (message "ptsus state %d proof %s sequent %s" state proof-name sequent-id)
  (let ((second-line
	 (format
	  (concat "update-sequent state %d sequent %s proof-name-bytes %d "
		  "sequent-text-bytes %d existential-bytes %d")
	  state sequent-id
	  (1+ (string-bytes proof-name))
	  (1+ (string-bytes sequent-text))
          (1+ (string-bytes existential-info)))))
    (proof-tree-send-message
     second-line
     (list proof-name sequent-text existential-info))))

(defun proof-tree-send-switch-goal (proof-state proof-name current-id)
  "Send switch-to command to prooftree."
  (let ((second-line
	 (format "switch-goal state %d sequent %s proof-name-bytes %d"
		 proof-state
		 current-id
		 (1+ (string-bytes proof-name)))))
    (proof-tree-send-message second-line (list proof-name))))

(defun proof-tree-send-branch-finished (state proof-name cmd-string
					     cheated-flag existential-info)
  "Send branch-finished to prooftree."
  (proof-tree-send-message
   (format
    (concat "branch-finished state %d %s proof-name-bytes %d command-bytes %d "
	    "existential-bytes %d")
    state
    (if cheated-flag "cheated" "not-cheated")
    (1+ (string-bytes proof-name))
    (1+ (string-bytes cmd-string))
    (1+ (string-bytes existential-info)))
   (list proof-name cmd-string existential-info)))

(defun proof-tree-send-proof-complete (state proof-name)
  "Send proof-complete to prooftree."
  (proof-tree-send-message
   (format "proof-complete state %d proof-name-bytes %d"
	   state
	   (1+ (string-bytes proof-name)))
   (list proof-name)))

(defun proof-tree-send-undo (proof-state)
  "Tell prooftree to undo."
  (let ((second-line (format "undo-to state %d" proof-state)))
    (proof-tree-send-message second-line nil)))

(defun proof-tree-send-quit-proof (proof-name)
  "Tell prooftree to close the window for PROOF-NAME."
  (let ((second-line (format "quit-proof proof-name-bytes %d"
			    (1+ (string-bytes proof-name)))))
    (proof-tree-send-message second-line (list proof-name))))


;;
;; Process output from the proof assistant
;;

(defun proof-tree-show-goal-callback (proof-name)
  "Callback for display-goal commands inserted by this package.
Update the sequent and run hooks in `proof-state-change-hook'.
Argument PROOF-NAME is necessary, because show goal commands can
be delayed until after the proof.

You CANNOT put this function directly as callback into
`proof-action-list' because callbacks receive the span as
argument and this function expects a string! Rather you should
call `proof-tree-make-show-goal-callback', which evaluates to a
lambda expressions that you can put into `proof-action-list'."
  (proof-tree-update-sequent proof-name)
  (run-hooks 'proof-state-change-pre-hook)
  (run-hooks 'proof-state-change-hook))

(defun proof-tree-make-show-goal-callback (proof-name)
  "Create the callback for display-goal commands."
  `(lambda (_span) (proof-tree-show-goal-callback ,proof-name)))

(defun proof-tree-quit-proof ()
  "Switch proof-tree display handling off inside Proof General."
  (setq proof-tree-current-proof nil)
  (proof-add-to-priority-queue (funcall proof-tree-display-stop-command)))

(defun proof-tree-check-proof-finish (last-item)
  "Urgent action to delay processing at proof end.
This function is called from `proof-shell-exec-loop' after the
old item has been removed and before the next item from
`proof-action-list' is sent to the proof assistant. Of course
only when the proof-tree display is active. At the end of the
proof, this function delays items just following the previous
proof until all show-goal commands from prooftree and the
`proof-tree-display-stop-command' (which switches the dependent
evar line off for Coq) have been processed.

If this function detects the end of the proof, it moves
non-priority items following in `proof-action-list' to
`proof-tree--delayed-actions' and sets
`proof-second-action-list-active'. When later the command from
`proof-tree-display-stop-command' is recognized, the items are
moved back. If no items follow the end of the previous proof,
`proof-tree-display-stop-command' is set to t."
  (cl-assert proof-tree-external-display nil
             "proof-tree-check-proof-finish precondition failure")
  ;; (message "PTCPF pt %s current %s mode %s delayed %s item %s"
  ;;          proof-tree-current-proof (cadr (funcall proof-tree-get-proof-info))
  ;;          proof-shell-busy proof-tree--delayed-actions
  ;;          last-item)
  (if (and proof-tree-current-proof
           ;; the current proof has been finished
           (not (cadr (funcall proof-tree-get-proof-info)))
           ;; it was not an undo aborting the proof
           (eq proof-shell-busy 'advancing)
           ;; first time we enter this
           (not proof-tree--delayed-actions))
      (let ((urgent-normal
             (list-pred-partition
              (lambda (action) (memq 'proof-tree-show-subgoal (nth 3 action)))
              proof-action-list)))
        (cl-assert (not proof-second-action-list-active) nil
                   "proof-tree-check-proof-finish: second action list active")
        (setq proof-action-list (car urgent-normal))
        (if (cdr urgent-normal)
            (setq proof-tree--delayed-actions (cdr urgent-normal))
          (setq proof-tree--delayed-actions t))
        (setq proof-second-action-list-active t))
    (when (and (memq 'proof-tree-last-item (nth 3 last-item))
               proof-tree--delayed-actions)
      (when proof-action-list
        (lwarn '(proof-tree) :warning
               "proof-action-list not empty when proof display is stopped"))
      (when (listp proof-tree--delayed-actions)
        (setq proof-action-list
              (nconc proof-action-list proof-tree--delayed-actions)))
      (setq proof-tree--delayed-actions nil)
      (setq proof-second-action-list-active nil)
      (setq proof-tree-current-proof nil)
      (setq proof-tree-external-display nil))))

(defun proof-tree-extract-goals (start end)
  "Extract the current goal state information from the delayed output.
If there is a current goal, return a list containing (in
this order) the ID of the current sequent, the text of the
current sequent and the list of ID's of additionally open goals.
The delayed output is expected between START and END in the
current buffer."
  (goto-char start)
  (if (proof-re-search-forward proof-tree-current-goal-regexp end t)
      (let ((sequent-id (match-string-no-properties 1))
	    (sequent-text (match-string-no-properties 2))
	    additional-goal-ids)
	(goto-char start)
	(while (proof-re-search-forward proof-tree-additional-subgoal-ID-regexp
					end t)
	  (let ((other-id (match-string-no-properties 1)))
	    (setq additional-goal-ids (cons other-id additional-goal-ids))))
	(setq additional-goal-ids (nreverse additional-goal-ids))
	(list sequent-id sequent-text additional-goal-ids))
    nil))


(defun proof-tree-extract-existential-info (start end)
  "Extract the information display of existential variables.
This function cuts out the text between
`proof-tree-existentials-state-start-regexp' and
`proof-tree-existentials-state-end-regexp' from the prover
output, including the matches of these regular expressions. If
the start regexp is nil, the empty string is returned. If the end
regexp is nil, the match expands to the end of the prover output."
  (goto-char start)
  (if (and proof-tree-existentials-state-start-regexp
	   (proof-re-search-forward proof-tree-existentials-state-start-regexp
				    end t))
      (progn
	(setq start (match-beginning 0))
	(when (and proof-tree-existentials-state-end-regexp
		   (proof-re-search-forward
		    proof-tree-existentials-state-end-regexp end t))
	  (setq end (match-end 0)))
	(buffer-substring-no-properties start end))
    ""))

(defun proof-tree-handle-proof-progress (old-proof-marker cmd-string proof-info)
  "Send CMD-STRING and goals in delayed output to prooftree.
This function is called if there is some real progress in a
proof. This function sends the current state, the current goal
and the list of additional open subgoals to Prooftree. Prooftree
will sort out the rest. In particular, Prooftree determines
without input from this function, whether or not a new layer in
the proof tree must be started.

The delayed output is in the region
\[proof-shell-delayed-output-start, proof-shell-delayed-output-end].
Urgent messages might be before that, following OLD-PROOF-MARKER,
which contains the position of `proof-marker', before the next
command was sent to the proof assistant."
  ;; (message "PTHPO cmd |%s| info %s flags %s proof-marker %s start %s end %s"
  ;; 	   cmd proof-info flags
  ;; 	   old-proof-marker
  ;; 	   proof-shell-delayed-output-start
  ;; 	   proof-shell-delayed-output-end)
  (let* ((start proof-shell-delayed-output-start)
	 (end   proof-shell-delayed-output-end)
	 (proof-state (car proof-info))
	 (proof-name (cadr proof-info))
	 (cheated-flag
	  (and proof-tree-cheating-regexp
	       (proof-string-match proof-tree-cheating-regexp cmd-string)))
	 current-goals)
    ;; Check first for special cases, because Coq's output for finished
    ;; branches is almost identical to proper goals.
    (goto-char old-proof-marker)
    (if (proof-re-search-forward proof-tree-branch-finished-regexp end t)
	(proof-tree-send-branch-finished
	 proof-state proof-name cmd-string cheated-flag
	 (proof-tree-extract-existential-info start end))
      (goto-char start)
      (setq current-goals (proof-tree-extract-goals start end))
      (when current-goals
	(let ((current-sequent-id (car current-goals))
	      (current-sequent-text (nth 1 current-goals)))
	  ;; Note that (nth 2 current-goals) contains the  additional ID's.
	  ;; Now send all to prooftree.
	  (proof-tree-send-goal-state
	   proof-state proof-name cmd-string
	   cheated-flag
	   current-sequent-id
	   current-sequent-text
	   (nth 2 current-goals)
	   (proof-tree-extract-existential-info start end)))))))

(defun proof-tree-handle-navigation (proof-info)
  "Handle a navigation command.
This function is called if there was a navigation command, which
results in a different goal being current now.

The delayed output of the navigation command is in the region
\[proof-shell-delayed-output-start, proof-shell-delayed-output-end]."
  (let ((start proof-shell-delayed-output-start)
	(end   proof-shell-delayed-output-end)
	(proof-state (car proof-info))
	(proof-name (cadr proof-info)))
    (goto-char start)
    (if (proof-re-search-forward proof-tree-current-goal-regexp end t)
	(let ((current-id (match-string-no-properties 1)))
	  ;; send all to prooftree
	  (proof-tree-send-switch-goal proof-state proof-name current-id)))))


(defun proof-tree-handle-proof-command (old-proof-marker cmd proof-info)
  "Display current goal in prooftree unless CMD should be ignored."
  ;; (message "PTHPC")
  (let (;; (proof-state (car proof-info))
	(cmd-string (mapconcat #'identity cmd " ")))
    (unless (and proof-tree-ignored-commands-regexp
		 (proof-string-match proof-tree-ignored-commands-regexp
				     cmd-string))
      (if (and proof-tree-navigation-command-regexp
	       (proof-string-match proof-tree-navigation-command-regexp
				   cmd-string))
	  (proof-tree-handle-navigation proof-info)
	(proof-tree-handle-proof-progress old-proof-marker
					  cmd-string proof-info)))
    (setq proof-tree-last-state (car proof-info))))
    
(defun proof-tree-handle-undo (proof-info)
  "Undo prooftree to current state."
  ;; (message "PTHU info %s" proof-info)
  (let ((proof-state (car proof-info)))
    (unless (equal (cadr proof-info) proof-tree-current-proof)
      ;; went back to a point before the start of the proof that we
      ;; are displaying;
      ;; or we have not started to display something
      (proof-tree-quit-proof))
    ;; disable proof tree display when undoing to a point outside a proof
    (unless proof-tree-current-proof
      (setq proof-tree-external-display nil))
    ;; send undo
    (if (proof-tree-is-running)
	(proof-tree-send-undo proof-state))
    (setq proof-tree-last-state proof-state)))


(defun proof-tree-update-sequent (proof-name)
  "Prepare an update-sequent command for prooftree.
This function processes delayed output that the proof assistant
generated in response to commands that prooftree inserted in
order to keep its display up-to-date.  Such commands are tagged
with a 'proof-tree-show-subgoal flag.  Argument PROOF-NAME
originally comes from prooftree and is passed back now, because
processing a show goal command might happen after the proof.

This function uses `proof-tree-update-goal-regexp' to find a
sequent, its ID and the corresponding state in the delayed
output. If something is found an appropriate update-sequent
command is sent to prooftree.

The delayed output is in the region
\[proof-shell-delayed-output-start, proof-shell-delayed-output-end]."
  ;; (message "PTUS buf %s output %d-%d state %s"
  ;; 	   (current-buffer)
  ;; 	   proof-shell-delayed-output-start proof-shell-delayed-output-end
  ;; 	   proof-state)
  (if (proof-tree-is-running)
      (with-current-buffer proof-shell-buffer
	(let* ((start proof-shell-delayed-output-start)
	       (end   proof-shell-delayed-output-end))
	  (goto-char start)
	  (if (proof-re-search-forward proof-tree-update-goal-regexp end t)
	      (let ((sequent-id (match-string-no-properties 1))
                    (proof-state
                     (string-to-number (match-string-no-properties 2)))
		    (sequent-text (match-string-no-properties 3)))
		(proof-tree-send-update-sequent
		 proof-state proof-name sequent-id sequent-text
                 (proof-tree-extract-existential-info start end))))))))

(defun proof-tree-handle-delayed-output (old-proof-marker cmd flags _span)
  "Process delayed output for prooftree.
This function is the main entry point of the Proof General
prooftree support.  It examines the delayed output in order to
take appropriate actions and maintains the internal state.

The delayed output to handle is in the region
\[proof-shell-delayed-output-start, proof-shell-delayed-output-end].
Urgent messages might be before that, following OLD-PROOF-MARKER,
which contains the position of `proof-marker', before the next
command was sent to the proof assistant.

All other arguments are (former) fields of the `proof-action-list'
entry that is now finally retired.  CMD is the command, FLAGS are
the flags and SPAN is the span."
  ;; (message "PTHDO cmd %s flags %s span %s-%s" cmd flags
  ;; 	   (if span (span-start span)) (if span (span-end span)))
  (cl-assert proof-tree-external-display)
  (unless (or (memq 'invisible flags) (memq 'proof-tree-show-subgoal flags))
    (let* ((proof-info (funcall proof-tree-get-proof-info))
	   (current-proof-name (cadr proof-info)))
      (save-excursion
	(if (< (car proof-info) proof-tree-last-state)
	    ;; went back to some old state: there must have been an undo command
            ;; show goal commands do not increase the state number,
            ;; therefore check with <
	    (proof-tree-handle-undo proof-info)

	  ;; else -- no undo command
	  ;; first maintain proof-tree-current-proof
	  (cond
	   ((and (null proof-tree-current-proof) current-proof-name)
	    ;; started a new proof
	    (setq proof-tree-current-proof current-proof-name))
	   ((and proof-tree-current-proof (null current-proof-name))
	    ;; signal prooftree that the proof is finished here and
	    ;; that we are waiting for confirmation
            (proof-tree-send-proof-complete (car proof-info)
                                            proof-tree-current-proof))
	   ((and proof-tree-current-proof current-proof-name
		 (not (equal proof-tree-current-proof current-proof-name)))
	    ;; new proof before old was finished?
	    (display-warning
	     '(proof-general proof-tree)
	     "Nested proofs are not supported in prooftree display"
	     :warning)
	    ;; try to keep consistency nevertheless
	    (setq proof-tree-current-proof current-proof-name)))

	  ;; send stuff to prooftree now
	  (when current-proof-name
	    ;; we are inside a proof: display something
	    (proof-tree-ensure-running)
	    (proof-tree-handle-proof-command old-proof-marker
					     cmd proof-info)))))))


;;
;; Send undo command when leaving a buffer
;;

(defun proof-tree-leave-buffer ()
  "Send an undo command to prooftree when leaving a buffer."
  (if (and proof-tree-configured (proof-tree-is-running))
      (proof-tree-send-undo 0)))

(add-hook 'proof-deactivate-scripting-hook #'proof-tree-leave-buffer)


;;
;; User interface
;;

(defun proof-tree-display-current-proof (proof-start)
  "Start an external proof-tree display for the current proof.
Coq (and probably other proof assistants as well) does not
support outputing the current proof tree.  Therefore this function
retracts to the start of the current proof, switches the
proof-tree display on, and reasserts then until the former end of
the locked region.  Argument PROOF-START must contain the start
position of the current proof."
  ;;(message "PTDCP %s" proof-tree-external-display)
  (unless (and proof-script-buffer
	       (equal proof-script-buffer (current-buffer)))
    (error
     "Enabling prooftree inside a proof outside the current scripting buffer"))
  (proof-shell-ready-prover)
  (cl-assert proof-locked-span)
  (message "Start proof-tree display for current proof")
  (save-excursion
    (save-window-excursion
      (let ((locked-end (span-end proof-locked-span)))
	(goto-char proof-start)
	;; enable external display to make sure the undo command is send
	(setq proof-tree-external-display t)
	(proof-retract-until-point)
	(while (consp proof-action-list)
	  (accept-process-output))
        (run-hooks 'proof-tree-start-display-hook)
	;; undo switched external display off; switch on again
	(setq proof-tree-external-display t)
	(goto-char locked-end)
	(proof-assert-until-point)
	(while (consp proof-action-list)
	  (accept-process-output))))))

(defun proof-tree-external-display-toggle ()
  "Toggle the external proof-tree display.
When called outside a proof the external proof-tree display will
be enabled for the next proof.  When called inside a proof the
proof display will be created for the current proof.  If the
external proof-tree display is currently on, then this toggle
will switch it off.  At the end of the proof the proof-tree
display is switched off."
  (interactive)
  (unless (and proof-tree-configured proof-tree-display-stop-command)
    (error "External proof-tree display not or not correctly configured for %s"
           proof-assistant))
  (cond
   (proof-tree-external-display
    ;; Currently on -> switch off
    (setq proof-tree-external-display nil)
    (when proof-tree-current-proof
      (proof-tree-send-quit-proof proof-tree-current-proof)
      (proof-tree-quit-proof))
    (message "External proof-tree display switched off"))
   (t
    ;; Currently off
    (let ((proof-start (funcall proof-tree-find-begin-of-unfinished-proof)))
      ;; ensure internal variables are initialized, because otherwise
      ;; we cannot process undo's after this
      (cl-assert (not proof-second-action-list-active) nil
                 "second action list active on prooftree start")
      (when proof-tree--delayed-actions
        (lwarn '(proof-tree) :warning
               "proof-tree--delayed-actions not empty on prooftree start"))
      (setq proof-tree--delayed-actions nil)
      (proof-tree-ensure-running)
      (setq proof-tree-current-proof nil)
      (setq proof-tree-last-state (car (funcall proof-tree-get-proof-info)))
      (if proof-start
	  ;; inside an unfinished proof -> start for this proof
	  (proof-tree-display-current-proof proof-start)
	;; outside a proof -> wait for the next proof
        (run-hooks 'proof-tree-start-display-hook)
	(setq proof-tree-external-display t)
	(proof-tree-send-undo proof-tree-last-state)
	(message
	 "External proof-tree display switched on for the next proof"))))))
    

;;
;; Trailer
;; 

(provide 'proof-tree)

;;; proof-tree.el ends here
