;;; proof-server.el --- Proof General server mode code

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

(require 'proof-config)
(require 'proof-proverargs)
(require 'proof-queue)
(require 'proof-buffers)
(require 'proof-script)

(defcustom proof-server-fiddle-frames t
  "Non-nil if proof-server functions should fire-up/delete frames like crazy."
  :type 'boolean
  :group 'proof-server)

(defvar proof-server-process nil)
(defvar proof-server-buffer nil)

(defvar proof-server-exit-in-progress nil
  "A flag indicating that the current proof process is about to exit.
This flag is set for the duration of `proof-server-kill-function'
to tell hooks in `proof-deactivate-scripting-hook' to refrain
from calling `proof-server-exit'.")

(defvar proof-server-delayed-output-flags nil)

(defconst proof-server-important-settings
  '(proof-server-send-to-prover-fun
    proof-server-make-command-thunk-fun
    ))

(defvar proof-last-span nil
  "Last span we've pulled off proof-action-list")

;; since commands are queued, we don't need to wait for a prompt
;; server is always available if process is running
(defun proof-server-available-p ()
  (and proof-server-process
       (eq (process-status proof-server-process) 'run)))

(defun proof-server-interrupt-process ()
  (interactive)
  ;; call prover's interrupter if available
  (if proof-server-interrupt-fun
      (funcall proof-server-interrupt-fun)
    (when proof-server-process
      ;; otherwise, send a SIGINT
      (interrupt-process proof-server-process t))))

(defun proof-server-clear-state ()
  "Clear internal state of proof shell."
  (setq proof-action-list nil
	;; proof-included-files-list nil  ??
	;; proof-nesting-depth 0 ??
	proof-prover-last-output ""))

;;;###autoload
(defsubst proof-server-live-buffer ()
  "Return non-nil if proof-server-buffer is live."
  (and proof-server-buffer
       (buffer-live-p proof-server-buffer)
       (let ((proc (get-buffer-process proof-server-buffer)))
	 (and proc (memq (process-status proc) '(open run stop))))))

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
	    (with-current-buffer proof-server-buffer
			 (add-hook 'kill-buffer-hook 'proof-server-kill-function t t))

	    ;; Send main intitialization command and wait for it to be
	    ;; processed.

	    ;; First, configure PGIP preferences (even before init cmd)
	    ;; available: this allows setting them after the init cmd.
	    ;; TODO ????
	    ;; (proof-maybe-askprefs)

	    ;; Now send the initialisation commands.
	    (unwind-protect
		(progn
		  (run-hooks 'proof-server-init-hook)
		  (if (stringp proof-server-init-cmd)
		      (proof-server-send-to-prover proof-server-init-cmd)
		    (dolist (cmd proof-server-init-cmd)
		      (proof-server-send-to-prover cmd)))
		  (if proof-assistant-settings
		      (mapcar (lambda (c)
				(proof-server-invisible-command c))
			      (proof-assistant-settings-cmds))))))))

;; use proof-action-list to create output 
(defun proof-server-manage-output (response)

  ;; code borrowed from proof-shell.el, proof-shell-filter-manage-output

  ;; A copy of the last message, verbatim, never modified.
  (setq proof-prover-last-output response)

  (when proof-action-list

    (let ((span  (caar proof-action-list))
	  (cmd   (nth 1 (car proof-action-list)))
	  (flags (nth 3 (car proof-action-list))))

      (proof-server-exec-loop)
      
      ;; TODO why is this after the loop
      '(if proof-tree-external-display
	   (proof-tree-handle-delayed-output old-proof-marker cmd flags span)))))

;;;###autoload
(defun proof-server-ready-prover (queuemode)
"Make sure the proof assistant is ready for a command.
We ignore QUEUEMODE, which is used just to give calling compatibility 
with proof-shell-ready-prover."
  (proof-server-start)
  proof-server-process)

(defun proof-server-sentinel (process event)
  (let ((prover (capitalize (symbol-name proof-assistant-symbol)))
	(cmd "To help diagnose the issue, enable logging with \"M-x proof-server-enable-logging\"."))
    (message-box "%s: %s\n\n%s" prover event cmd)
    (proof-server-restart)))

;;;###autoload
(defun proof-server-start ()
  (interactive)
  (unless (and proof-server-process (eq (process-status proof-server-process) 'run))
    (let* ((command-line-and-names (prover-command-line-and-names)) 
	   (prog-command-line (car command-line-and-names))
	   (prog-name-list (cdr command-line-and-names))
	   (redirect (if (eq system-type 'windows-nt)
			     "2> NUL"
		       "2> /dev/null"))
	   ;; leading space hides the buffer
	   (server-buffer (get-buffer-create (concat " *" proof-assistant "*")))
	   (the-process
	    (let ((curr-proc-conn-type process-connection-type))
	      (setq process-connection-type nil)
	      (unwind-protect 
		  (apply 'start-process-shell-command
			 (cons proof-assistant
			       (cons server-buffer
				     (append prog-command-line (list redirect)))))
		(setq process-connection-type curr-proc-conn-type)))))
      (if the-process
	  (progn 
	    (set-process-sentinel the-process 'proof-server-sentinel)
	    (setq proof-server-process the-process
		  proof-server-buffer server-buffer)
	    (proof-prover-make-associated-buffers)
	    (proof-server-config-done))
	(message-box "Failed to start prover with command line: \"%s\"" prog-command-line)))
    (when proof-server-fiddle-frames
      (save-selected-window
	(save-selected-frame
	 (proof-multiple-frames-enable))))))

;;;###autoload
(defun proof-server-invisible-command (cmd)
  (proof-server-send-to-prover cmd))

;;;###autoload
(defun proof-server-invisible-cmd-handle-result (cmd handler)
  (proof-server-send-to-prover cmd handler))

;;;###autoload
(defun proof-server-invisible-command-invisible-result (cmd)
  (proof-server-invisible-command cmd))

;;;###autoload
(defun proof-server-insert (strings action span)
  "Send STRINGS to the prover.

STRINGS is a list of strings (which will be concatenated), or a
single string.

The ACTION may be used by the hook, while SPAN is the Emacs span containing the command."
  (cl-assert (or (stringp strings)
	      (listp strings))
	  nil "proof-server-insert: expected string or list argument")
  (let ((string (if (stringp strings) strings
		  (apply 'concat strings))))
    (run-hooks 'proof-server-insert-hook)
    (proof-server-queue-command-and-span-for-prover string span)))

(defun proof-server-queue-command-and-span-for-prover (string span)
  (let ((thunk (funcall proof-server-make-command-thunk-fun string span)))
    (proof-server-send-to-prover thunk)))

(defsubst proof-server-insert-action-item (item)
  "Send ITEM from `proof-action-list' to prover."
  (proof-server-insert (nth 1 item) (nth 2 item) (nth 0 item)))

;;;###autoload
(defun proof-server-add-to-queue (queue-items &optional queuemode)
  "add item to queue for 'server mode"

  (let ((nothingthere (null proof-action-list)))
    ;; Now extend or start the queue.
    (setq proof-action-list
	  (nconc proof-action-list queue-items))

    (run-hook-with-args 'proof-server-enqueue-hook queue-items)

    (when nothingthere ; process comments immediately
      (let ((cbitems (proof-prover-slurp-comments)))
	(mapc 'proof-prover-invoke-callback cbitems)))
    
    ; in proof shell, have stuff about silent mode
    ; not relevant in server mode
    (if proof-action-list ;; something to do
	(progn
	  (when nothingthere  ; start sending commands
	    '(proof-grab-lock queuemode)
	    (setq proof-prover-last-output-kind nil) 
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

	(when proof-action-list
	  ;; send the next command to the process.
	  (proof-server-insert-action-item (car proof-action-list)))

	;; process the delayed callbacks now
	(mapc 'proof-prover-invoke-callback cbitems)	

	(unless (or proof-action-list proof-second-action-list-active)
	; release lock, cleanup
	  '(proof-release-lock) ;; TODO ????
	  (proof-detach-queue)
	  (unless flags ; hint after a batch of scripting
	    (pg-processing-complete-hint)))
	(and (not proof-second-action-list-active)
	     (or (null proof-action-list)
		 ;; TODO when proof tree working again
		 '(cl-every
		  (lambda (item) (memq 'proof-tree-show-subgoal (nth 3 item)))
		  proof-action-list)))))))

(defun proof-server-kill-function ()
  "Function run when a proof-server buffer is killed.
Try to shut down the proof process nicely and clear locked
regions and state variables.  Value for `kill-buffer-hook' in
shell buffer, called by `proof-shell-bail-out' if process exits."
  (let* ((proc     (get-buffer-process (current-buffer)))
	 (bufname  (buffer-name)))
    (message "%s, cleaning up and exiting..." bufname)

    ;; sometimes causes internal Emacs 24.5 error
    ;; (redisplay t)

    (when proc

      (catch 'exited
	(setq proof-server-exit-in-progress t)
	(set-process-sentinel 
	 proc
	 (lambda (p m) (throw 'exited t)))
	
	;; Turn off scripting (ensure buffers completely processed/undone)
	(proof-deactivate-scripting-auto)

	;; Try to shut down politely, if have full last response from prover
	(when (proof-server-response-complete)
	  (if proof-server-quit-cmd
	      (progn
		(proof-server-send-to-prover proof-server-quit-cmd)
		(accept-process-output proc 0 100))
	    (process-send-eof))
	  
	  ;; Wait for it to die
	  (let ((timecount   (proof-ass quit-timeout))
		(proc        (get-buffer-process proof-server-buffer)))
	    (while (and proc
			(> timecount 0)
			(memq (process-status proc) '(open run stop)))
	      (accept-process-output proc 1 nil 1)
	      (cl-decf timecount))))

	;; Still there, kill it rudely.
	(when (memq (process-status proc) '(open run stop))
	  (message "%s, cleaning up and exiting...killing process" bufname)
	  (kill-process proc)))

      (setq proof-server-process nil)
      (set-process-sentinel proc nil))

    ;; Clear all state
    (proof-script-remove-all-spans-and-deactivate)
    (proof-server-clear-state)

    ;; Log death
    (proof-server-log (format "** Proof General: End of %s session ***" proof-assistant) "\n")
    
    ;; Remove auxiliary windows, trying to stop proliferation of 
    ;; frames (NB: loses if user has switched buffer in special frame)
    (if (and proof-multiple-frames-enable
	     proof-server-fiddle-frames)
	(proof-delete-all-associated-windows))

    ;; Kill associated buffer
    (let ((proof-server-buffer nil)) ;; fool kill buffer hooks
      (dolist (buf '(proof-goals-buffer proof-response-buffer))
	(when (buffer-live-p (symbol-value buf))
	  ;; allow killing buffer
	  (with-current-buffer (symbol-value buf)
	    (remove-hook 'kill-buffer-hook 'pg-save-from-death t))
	  (delete-windows-on (symbol-value buf))
	  (kill-buffer (symbol-value buf))
	  (set buf nil))))

    (setq proof-server-buffer nil)
    (setq proof-server-exit-in-progress nil)
    (message "%s exited." bufname)))

(defun proof-server-exit (&optional dont-ask)
  "Query the user and exit the proof process.

This simply kills the `proof-server-buffer' relying on the hook function
`proof-server-kill-function' to do the hard work. If optional
argument DONT-ASK is non-nil, the proof process is terminated
without confirmation.

The kill function uses `<PA>-quit-timeout' as a timeout to wait
after sending `proof-server-quit-cmd' before rudely killing the process.

This function should not be called if
`proof-server-exit-in-progress' is t, because a recursive call of
`proof-server-kill-function' will give strange errors."
  (interactive "P")
  (if (buffer-live-p proof-server-buffer)
      (when (or dont-ask
		(yes-or-no-p (format "Exit %s process? " proof-assistant)))
	(let ((kill-buffer-query-functions nil)) ; avoid extra dialog
	  (kill-buffer proof-server-buffer))
	(setq proof-server-buffer nil))
    (error "No proof server buffer to kill!")))

(defun proof-server-restart ()
  "Restart proof server."
  (interactive)
  (run-hooks 'proof-server-restart-hook)
  (proof-server-exit t))

;; TODO move these to a good home
(defun proof-check ()
  "Check validity of entire document."
  (interactive)
  (when proof-check-command
    (funcall proof-check-command)))

(defun proof-check-available-p ()
  (and proof-server-process
       proof-check-command))

(defun proof-find-theorems ()
  "Find theorems containing item."
  (interactive)
  (when proof-find-theorems-command
    (funcall proof-find-theorems-command)))

(defun proof-get-context ()
  "Get current proof context."
  (interactive)
  (when proof-context-command
    (proof-invisible-command
     (funcall proof-context-command))))

(defun proof-context-available-p ()
  (and proof-server-process
       proof-context-command))
  
(provide 'proof-server)

;; proof-server.el ends here
