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


(defun proof-context-available-p ()
  (and proof-server-process
       proof-context-command))
  
(provide 'coq-xml)

;; coq-xml.el ends here
