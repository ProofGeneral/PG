;; ;;; proof-server.el --- Proof General server mode code
;;

(require 'pg-response)
(require 'proof-config)
(require 'proof-proverargs)
(require 'proof-queue)
(require 'proof-buffers)

(defvar proof-server-process nil)

(defconst proof-server-important-settings
  '(proof-server-send-to-prover-fun
    proof-server-process-response-fun
    ))

(defvar proof-server-delayed-output-flags nil
  "A copy of the `proof-action-list' flags for `proof-server-delayed-output'.")

(defun proof-server-log (src str)
  (with-current-buffer proof-server-log-buffer
    (insert "*" src ">>>" str "<<<" src "*\n")))

;;;###autoload
(defun proof-server-config-done ()
  "Initialise the specific prover after the child has been configured.
When using server mode, should call this function at the end of processing. 
For shell modes, the config-done procedure is called when instantiating an 
derived Emacs mode; here, we call the procedure directly."
  (dolist (sym proof-server-important-settings)
    (proof-warn-if-unset "proof-server-config-done" sym))

  (if (memq (process-status proof-server-process) '(open run))
	  (progn
	    ;; Also ensure that proof-action-list is initialised.
	    (setq proof-action-list nil)
	    ;; Send main intitialization command and wait for it to be
	    ;; processed.

	    ;; First, configure PGIP preferences (even before init cmd)
	    ;; available: this allows setting them after the init cmd.
;; TODO ????
;;	    (proof-maybe-askprefs)

	    ;; Now send the initialisation commands.
	    (unwind-protect
		(progn
		  (run-hooks 'proof-server-init-hook)
		  (when proof-server-init-cmd
		    (if (listp proof-server-init-cmd)
			(mapc 'proof-server-invisible-command-invisible-result
				proof-server-init-cmd)
		      (proof-server-invisible-command-invisible-result 
		       proof-server-init-cmd)))
		  (if proof-assistant-settings
		      (mapcar (lambda (c)
				(proof-server-invisible-command c t))
			      (proof-assistant-settings-cmds))))))))

;;;###autoload
(defun proof-server-filter (process response)
  "The filter function associated with the prover process.
The first argument is the process object, the second is the response from the prover."
  (message "Called server filter")
  (message (concat "Message is " response))

  (if proof-server-log-traffic
      (proof-server-log proof-assistant response))

  ; parse response in prover-specific way
  (proof-server-process-response response)
  ; take care of display buffers
  (proof-server-filter-manage-output response))

;; use proof-action-list to create output 
(defun proof-server-filter-manage-output (response)

  ;; code borrowed from proof-shell.el, proof-shell-filter-manage-output

  (let ((span  (caar proof-action-list))
	(cmd   (nth 1 (car proof-action-list)))
	(flags (nth 3 (car proof-action-list))))

    ;; A copy of the last message, verbatim, never modified.
    (setq proof-prover-last-output response)

    ;; process response, run main loop
    ;; TODO we have a single function for dealing with output, rather than 
    ;;  immediate and delayed function as in the proof shell
    ;; maybe that will change
    (let ((output-kind (proof-server-handle-output cmd flags)))
      (setq proof-prover-last-output-kind output-kind))

    (proof-server-exec-loop)
    
    ;; TODO why is this after the loop
    '(if proof-tree-external-display
	(proof-tree-handle-delayed-output old-proof-marker cmd flags span))))

;;;###autoload
(defun proof-server-ready-prover (queuemode)
  "Compare with proof-shell-ready-prover, for proof shells. 
Make sure the proof assistant is ready for a command.
We ignore QUEUEMODE, which is used just to give calling compatibility 
with proof-shell-ready-prover."
  (message "Called proof-server-ready-prover")
  (proof-server-start)
;  (error "Could not start prover")
  t)

;;;###autoload
(defun proof-server-start ()
  (message "Called proof-server-start")
  (let* ((command-line-and-names (prover-command-line-and-names))
	 (prog-command-line (car command-line-and-names))
	 (prog-name-list (cdr command-line-and-names)))

    (message "Starting: %s" prog-command-line)
    (message "Program is: %s" proof-assistant-symbol)
    (message "Command line: %s" prog-command-line)
    (message "Proof assistant: %s" proof-assistant)
    (message "Prog name list: %s" prog-name-list)
    (unless proof-server-process
      (let* ((server-buffer (get-buffer-create (concat "*" proof-assistant "*")))
	     (the-process (apply 'start-process (cons proof-assistant (cons server-buffer prog-command-line)))))
	(if the-process
	  (progn 
	    (message "Started prover process")
	    (setq proof-server-process the-process
		  proof-server-buffer server-buffer)
	    (set-process-filter proof-server-process 'proof-server-filter)
	    (proof-prover-make-associated-buffers)
	    (proof-server-config-done))
	  (message "Failed to start prover"))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Processing error output
;;

;; TODO: this is based on shell equivalent, need to make sure it's sane
(defun proof-server-handle-error (err flags)
  "React on an error or interrupt message triggered by the prover.

The argument ERR-OR-INT should be set to 'error or 'interrupt
which affects the action taken.

For errors, we first flush unprocessed output (usually goals).
The error message is the (usually) displayed in the response buffer.

For interrupts, a warning message is displayed.

In both cases we then sound a beep, clear the queue and spans."
  (unless (memq 'no-error-display flags)
      (message "proof-server-handle-error")))

(defun proof-server-handle-interrupt (interrupt flags)
  "React on an error or interrupt message triggered by the prover.

The argument ERR-OR-INT should be set to 'error or 'interrupt
which affects the action taken.

For errors, we first flush unprocessed output (usually goals).
The error message is the (usually) displayed in the response buffer.

For interrupts, a warning message is displayed.

In both cases we then sound a beep, clear the queue and spans."
  (unless (memq 'no-error-display flags)
    (message "proof-server-handle-interrupt")
    (pg-response-maybe-erase t t t) ; force cleaned now & next
    (pg-response-warning
     "Interrupt: script management may be in an inconsistent state
	   (but it's probably okay)")))

(defun proof-server-handle-output (output flags)
  "React on an error or interrupt message triggered by the prover.

The argument ERR-OR-INT should be set to 'error or 'interrupt
which affects the action taken.

For errors, we first flush unprocessed output (usually goals).
The error message is the (usually) displayed in the response buffer.

For interrupts, a warning message is displayed.

In both cases we then sound a beep, clear the queue and spans."
  (unless (memq 'no-error-display flags)
    (message "proof-server-handle-output")
    ;; TODO this is where we parse XML, dispatch to goals and response
))

;;;###autoload
(defun proof-server-handle-immediate-output (cmd flags)
  "See if the output in cmd must be dealt with immediately.
To speed up processing, PG tries to avoid displaying output that
the user will not have a chance to see.  Some output must be
handled immediately, however: these are errors, interrupts,
goals and loopbacks (proof step hints/proof by pointing results).

In this function we check, in turn:

  `proof-server-interruptp'
  `proof-server-errorp'
  `proof-server-proof-completedp'
  `proof-server-result-startp' TODO ??

These are predicates on cmd, supplied by the prover configuration. 
Compare with the proof shell approach, which looks for regexp matches on text.

To extend this, set `proof-server-handle-output-system-specific',
which is a hook to take particular additional actions.

This function sets variables: `proof-prover-last-output-kind',
and the counter `proof-prover-proof-completed' which counts commands
after a completed proof."
  (message "Called proof-server-handle-immediate-output")

  (setq proof-prover-last-output-kind nil) ; unclassified

  (cond
   ((proof-server-interruptp cmd)
    (setq proof-prover-last-output-kind 'interrupt)
    (proof-server-handle-interrupt cmd flags))
   
   ((proof-server-errorp cmd)
    (setq proof-prover-last-output-kind 'error)
    (proof-server-handle-error cmd flags))

;   ((proof-server-result-startp cmd)
;    ???? TODO
   
   ((proof-server-proof-completedp cmd)
    (setq proof-prover-proof-completed 0)))

  ;; PG4.0 change: simplify and run earlier
  (if proof-server-handle-output-system-specific
      (funcall proof-server-handle-output-system-specific
	       cmd proof-prover-last-output))
  )

;;;###autoload
(defun proof-server-handle-delayed-ouput (cmd flags)
  (message "Called proof-server-handle-delayed-output")
)

;;; assume that cmd is already formatted appropriately 
;;;###autoload
(defun proof-server-invisible-command (cmd &optional wait invisiblecallback
					   &rest flags)
  (message (format "Called proof-server-invisible-command: %s" cmd))
  (proof-server-send-to-prover cmd)
  ;; TODO deal with wait, callback, flags
)

;;;###autoload
(defun proof-server-invisible-cmd-get-result (cmd)
  ;; TODO prover-specific wrapping function for cmd and result
  ;; e.g. in Coq, wrap cmd in XML
  (message "Called proof-server-invisible-command-get-result")
  (proof-server-invisible-command cmd 'waitforit
				 nil
				 'no-response-display
				 'no-error-display)
  proof-prover-last-output)

;;;###autoload
(defun proof-server-invisible-command-invisible-result (cmd)
  ;; TODO prover-specific wrapping function for cmd
  ;; e.g. in Coq, wrap cmd in XML
  (message "Called proof-server-invisible-command-get-invisible-result")
  (proof-server-invisible-command cmd 'waitforit
				 nil
				 'no-response-display
				 'no-error-display))

;;;###autoload
(defun proof-server-add-to-queue (queueitems &optional queuemode)
  "add item to queue for 'server mode"
  (message "Called proof-server-add-to-queue")
  (message (format "Items: %s" queueitems))
  t)

(defun proof-server-format-item (item)
  "TODO: fill in")

;;;###autoload
(defun proof-server-insert-action-item (item)
  "Send ITEM from `proof-action-list' to prover."
  (message "proof-server-insert-action-item")
  (let ((formatted-item (proof-server-format-item item)))
    (process-send-string proof-server-process formatted-item)))

;;; TODO: factor out common code with proof shell exec loop

;;;###autoload
(defun proof-server-exec-loop ()
  "Main loop processing the `proof-action-list', called from server process filter.

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
contains only invisible elements for Prooftree synchronization."
  (unless (null proof-action-list)
    (save-excursion
      (if proof-script-buffer		      ; switch to active script
	  (set-buffer proof-script-buffer))

      (let* ((item    (car proof-action-list))
	     (flags   (nth 3 item))
	     cbitems)

	;; now we should invoke callback on just processed command,
	;; but we delay this until sending the next command, attempting
	;; to parallelize prover and Emacs somewhat.  (PG 4.0 change)

	(setq proof-action-list (cdr proof-action-list))

	(setq cbitems (cons item
			    (proof-prover-slurp-comments)))

	;; This is the point where old items have been removed from
	;; proof-action-list and where the next item has not yet been
	;; sent to the proof assistant. This is therefore one of the
	;; few points where it is safe to manipulate
	;; proof-action-list. The urgent proof-tree display actions
	;; must therefore be called here, because they might add some
	;; Show actions at the front of proof-action-list.
;	(if proof-tree-external-display
;	    (proof-tree-urgent-action flags))

	;; if action list is (nearly) empty, ensure prover is noisy.
;	(if (and proof-shell-silent
;		 (not (eq (nth 2 item) 'proof-shell-clear-silent))
;		 (or (null proof-action-list)
;		     (null (cdr proof-action-list))))
	    ;; Insert the quieten command on head of queue
;	    (setq proof-action-list
;		  (cons (proof-shell-stop-silent-item)
;			proof-action-list)))

	;; pending interrupts: we want to stop the queue here
;       (when proof-shell-interrupt-pending
;	  (mapc 'proof-shell-invoke-callback cbitems)
;	  (setq cbitems nil)
;	  (proof-shell-handle-error-or-interrupt 'interrupt flags))

	(if proof-action-list
	    ;; send the next command to the process.
	    (proof-server-insert-action-item (car proof-action-list)))

	;; process the delayed callbacks now
	(mapc 'proof-server-invoke-callback cbitems)	

	(unless (or proof-action-list proof-second-action-list-active)
	; release lock, cleanup
	  '(proof-release-lock) ; TODO
	  (proof-detach-queue)
	  (unless flags ; hint after a batch of scripting
	    (pg-processing-complete-hint)))

	(and (not proof-second-action-list-active)
	     (or (null proof-action-list)
		 (every
		  (lambda (item) (memq 'proof-tree-show-subgoal (nth 3 item)))
		  proof-action-list)))))))

(provide 'proof-server)

;;; proof-server.el ends here
