;; ;;; proof-server.el --- Proof General server 
;;
;;  proof-server-ready-prover
;;    starts prover, gives error if it's busy.

(require 'proof-config)
(require 'proof-proverargs)

(defvar proof-server-process nil)

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
    (let ((the-process (apply 'start-process (cons proof-assistant (cons (concat "*" proof-assistant "*") prog-command-line)))))
      (if (null the-process)
	  (progn
	    (message "Unable to start prover process")
	    (error "Unable to start prover process"))
	  (progn 
	    (message "Started prover")
	    (setq proof-server-process the-process))))
    ; set filter function, if it exists
    (if (not (null proof-server-filter-fun))
	(set-process-filter proof-server-process proof-server-filter-fun))
    ; initialize prover
    (if (stringp proof-server-init-cmd)
	(process-send-string proof-server-process proof-server-init-cmd)
      (if (listp proof-server-init-cmd)
	(mapc (lambda (cmd) 
		(process-send-string proof-server-process cmd))
	      proof-server-init-cmd)))
))	   

;;;###autoload
(defun proof-server-invisible-command (cmd &optional wait invisiblecallback
					   &rest flags)
  ;; TODO prover-specific wrapping function for cmd
  ;; e.g. in Coq, wrap cmd in XML
  (message "Called proof-server-invisible-command")
  t)

;;;###autoload
(defun proof-server-invisible-cmd-get-result (cmd)
  ;; TODO prover-specific wrapping function for cmd and result
  ;; e.g. in Coq, wrap cmd in XML
  (message "Called proof-server-invisible-command-get-result")
  t)

;;;###autoload
(defun proof-server-invisible-command-invisible-result (cmd)
  ;; TODO prover-specific wrapping function for cmd
  ;; e.g. in Coq, wrap cmd in XML
  (message "Called proof-server-invisible-command-get-invisible-result")
  t)

;;;###autoload
(defun proof-server-add-to-queue (queueitems &optional queuemode)
  "add item to queue for 'server mode"
  (message "Called proof-server-add-to-queue")
  t)

(provide 'proof-server)

;;; proof-server.el ends here
