;; ;;; proof-server.el --- Proof General server mode code
;;

(require 'pg-response)
(require 'proof-config)
(require 'proof-proverargs)
(require 'proof-queue)
(require 'proof-buffers)

(defcustom proof-server-fiddle-frames t
  "Non-nil if proof-server functions should fire-up/delete frames like crazy."
  :type 'boolean
  :group 'proof-server)

(defvar proof-server-process nil)

(defvar proof-server-delayed-output-flags nil)

(defconst proof-server-important-settings
  '(proof-server-send-to-prover-fun
    ))

(defvar proof-last-span nil
  "Last span we've pulled off proof-action-list")

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

;; use proof-action-list to create output 
(defun proof-server-manage-output (response)

  ;; code borrowed from proof-shell.el, proof-shell-filter-manage-output

  (let ((span  (caar proof-action-list))
	(cmd   (nth 1 (car proof-action-list)))
	(flags (nth 3 (car proof-action-list))))

    ;; A copy of the last message, verbatim, never modified.
    ;; TODO do we really need this?
    (setq proof-prover-last-output response)

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
  t)

;;;###autoload
(defun proof-server-start ()
  (message "Called proof-server-start")
  (unless proof-server-process
    (let* ((command-line-and-names (prover-command-line-and-names))
	   (prog-command-line (car command-line-and-names))
	   (prog-name-list (cdr command-line-and-names)))
      (message "Starting: %s" prog-command-line)
      (message "Program is: %s" proof-assistant-symbol)
      (message "Command line: %s" prog-command-line)
      (message "Proof assistant: %s" proof-assistant)
      (message "Prog name list: %s" prog-name-list)
      (let* ((server-buffer (get-buffer-create (concat "*" proof-assistant "*")))
	     (the-process (apply 'start-process (cons proof-assistant (cons server-buffer prog-command-line)))))
	(if the-process
	    (progn 
	      (message "Started prover process")
	      (setq proof-server-process the-process
		    proof-server-buffer server-buffer)
	      ;; NB we don't associate a filter with process here
	      (proof-prover-make-associated-buffers)
	      (proof-server-config-done))
	  (message "Failed to start prover"))))
    (when proof-server-fiddle-frames
      (save-selected-window
	(save-selected-frame
	 (proof-multiple-frames-enable))))))

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
(defun proof-server-insert (strings action &optional scriptspan)
  "Send STRINGS to the prover.

STRINGS is a list of strings (which will be concatenated), or a
single string.

The ACTION and SCRIPTSPAN arguments are here to conform to `proof-shell-insert''s API."
  (assert (or (stringp strings)
	      (listp strings))
	  nil "proof-server-insert: expected string or list argument")
  (run-hooks 'proof-server-insert-hook)

  (let ((string (if (stringp strings) strings
		  (apply 'concat strings))))
    ;; t means string should be formatted for prover
    (proof-server-send-to-prover string t)))

(defsubst proof-server-insert-action-item (item)
  "Send ITEM from `proof-action-list' to prover."
  (message "proof-server-insert-action-item: %s" item)
  (proof-server-insert (nth 1 item) (nth 2 item) (nth 0 item)))

;;;###autoload
(defun proof-server-add-to-queue (queueitems &optional queuemode)
  "add item to queue for 'server mode"
  '(message "Called proof-server-add-to-queue")
  '(message (format "Items: %s" queueitems))

  (let ((nothingthere (null proof-action-list)))
    ;; Now extend or start the queue.
    (setq proof-action-list
	  (nconc proof-action-list queueitems))

    (when nothingthere ; process comments immediately
      (let ((cbitems  (proof-prover-slurp-comments))) 
	(message "Calling callback on items %s" cbitems)
	(mapc 'proof-prover-invoke-callback cbitems))) 
    ; in proof shell, have stuff about silent mode
    ; not relevant in server mode
    (if proof-action-list ;; something to do
	(progn
	  (message "Nonempty proof-action-list")
	  (when nothingthere  ; start sending commands
	    '(proof-grab-lock queuemode)
	    (setq proof-prover-last-output-kind nil) 
	    (message "INSERTING 1")
	    (proof-server-insert-action-item (car proof-action-list))))
      (if proof-second-action-list-active
	  ;; primary action list is empty, but there are items waiting
	  ;; somewhere else
	  '(proof-grab-lock queuemode)
      ;; nothing to do: maybe we completed a list of comments without sending them
	(proof-detach-queue)))))

;;; TODO: factor out common code with proof shell exec loop

;;  notes by Paul Steckler
;;  - the proof-action-list is a list of data structures with text and a callback, and some other bits
;;  - it's extended by calling proof-add-to-queue
;;  - if the queue had been empty, proof-add-queue send the text to the prover buffer, which has the effect of sending it to the prover itself; the items remain on proof-action-list
;;  - in the exec loop, the first item is picked off for processing, then the next item is sent to the shell; the picked-off item has its callback invoked

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
  (message "called proof-server-exec-loop")
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

	(setq proof-last-span (car item))

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
	    (progn
	      (message "INSERTING 2")
	      (proof-server-insert-action-item (car proof-action-list))))

	;; process the delayed callbacks now
	(mapc 'proof-prover-invoke-callback cbitems)	

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
