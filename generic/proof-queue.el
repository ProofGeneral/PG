;;; proof-queue.el -- queue management
;;;
;; this code should be independent of 'repl or 'server mode

(require 'proof-script)
(require 'proof-resolver)

(defvar proof-action-list nil
  "The main queue of things to do: spans, commands and actions.
The value is a list of lists of the form

   (SPAN COMMANDS ACTION [DISPLAYFLAGS])

which is the queue of things to do.

SPAN is a region in the sources, where COMMANDS come from. Often,
additional properties are recorded as properties of SPAN.

COMMANDS is a list of strings, holding the text to be send to the
prover. It might be the empty list if nothing needs to be sent to
the prover, such as, for comments. Usually COMMANDS
contains just 1 string, but it might also contains more elements.
The text should be obtained with
`(mapconcat 'identity COMMANDS \" \")', where the last argument
is a space. For server mode, the COMMANDS may be more complex than 
the raw text from the proof script, such as XML.

ACTION is the callback to be invoked when this item has been
processed by the prover. For normal scripting items it is
`proof-done-advancing', for retract items
`proof-done-retracting', but there are more possibilities (e.g.
`proof-done-invisible', `proof-shell-set-silent',
`proof-shell-clear-silent' and `proof-tree-show-goal-callback').

The DISPLAYFLAGS are set
for non-scripting commands or for when scripting should not
bother the user.  They may include

  'invisible		    non-script command (`proof-shell-invisible-command')
  'no-response-display      do not display messages in *response* buffer
  'no-error-display         do not display errors/take error action
  'no-goals-display         do not goals in *goals* buffer
  'proof-tree-show-subgoal  item inserted by the proof-tree package

Note that 'invisible does not imply any of the others. If flags
are non-empty, interactive cues will be surpressed. (E.g.,
printing hints).

See the functions `proof-start-queue' and `proof-shell-exec-loop'.")

(defvar proof-second-action-list-active nil
  "Signals that some items are waiting outside of `proof-action-list'.
If this is t it means that some items from the queue region are
waiting for being processed in a place different from
`proof-action-list'. In this case Proof General must behave as if
`proof-action-list' would be non-empty, when it is, in fact,
empty.

This is used, for instance, for parallel background compilation
for Coq: The Require command and the following items are not put
into `proof-action-list' and are stored somewhere else until the
background compilation finishes. Then those items are put into
`proof-action-list' for getting processed.")

(defsubst proof-prover-invoke-callback (listitem)
  "From `proof-action-list' LISTITEM, invoke the callback on the span."
  (condition-case nil
      (funcall (nth 2 listitem) (car listitem))
    (error nil)))

(defsubst proof-prover-slurp-comments ()
  "Strip comments at front of `proof-action-list', returning items stripped.
Comments are not sent to the prover."
  (let (cbitems nextitem)
    (while (and proof-action-list
		(not (nth 1 (setq nextitem
				  (car proof-action-list)))))
      (setq cbitems (cons nextitem cbitems))
      (setq proof-action-list (cdr proof-action-list)))
    (nreverse cbitems)))

;;;###autoload
(defun proof-start-queue (start end queueitems &optional queuemode)
  "Begin processing a oqueue of commands in QUEUEITEMS.
If START is non-nil, START and END are buffer positions in the
active scripting buffer for the queue region.

This function calls `proof-add-to-queue'."
  '(if start
      (proof-set-queue-endpoints start end))
  (proof-add-to-queue queueitems queuemode))

;;;###autoload
(defun proof-extend-queue (end queueitems)
  "Extend the current queue with QUEUEITEMS, queue end END.
To make sense, the commands should correspond to processing actions
for processing a region from (buffer-queue-or-locked-end) to END.
The queue mode is set to 'advancing"
  (proof-set-queue-endpoints (proof-unprocessed-begin) end)
  (condition-case err
      (run-hooks 'proof-extend-queue-hook)
    ((error quit)
      (proof-detach-queue)
      (signal (car err) (cdr err))))
  (proof-add-to-queue queueitems 'advancing))

(provide 'proof-queue)

;;; proof-queue.el ends here
