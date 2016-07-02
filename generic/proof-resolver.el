;;; proof-resolver.el --- dispatch to functions stored in variables
;;
;;; Code:

(require 'pg-vars)
(require 'proof-config)

(defun proof-server-log (src str)
  (with-current-buffer proof-server-log-buffer
    (insert "*" src ">>>" str "<<<" src "*\n")))

;;;###autoload
(defun proof-ready-prover (&optional queuemode)
  (funcall proof-ready-prover-fun queuemode))

;;;###autoload
(defun proof-invisible-command (cmd &optional wait invisiblecallback
				    &rest flags)
  (funcall proof-invisible-command-fun cmd wait invisiblecallback flags))

;;;###autoload
(defun proof-invisible-cmd-get-result (cmd)
  (funcall proof-invisible-cmd-get-result-fun cmd))

;;;###autoload
(defun proof-invisible-command-invisible-result (cmd)
  (funcall proof-invisible-command-invisible-result-fun cmd))

;;;###autoload
(defun proof-add-to-queue (queueitems &optional queuemode)
  (funcall proof-add-to-queue-fun queueitems queuemode))

;;;###autoload
(defun proof-server-interruptp (resp)
  (and proof-server-interruptp-fun 
       (funcall proof-server-interruptp-fun resp)))

;;;###autoload
(defun proof-server-errorp (resp)
  (and proof-server-errorp-fun 
       (funcall proof-server-errorp-fun resp)))

;;;###autoload
(defun proof-server-proof-completedp (resp)
  (and proof-server-proof-completedp-fun 
       (funcall proof-server-proof-completedp-fun resp)))

;;;###autoload
(defun proof-server-format-command (cmd)
  (and proof-server-format-command-fun 
       (funcall proof-server-format-command-fun cmd)))

;;;###autoload
(defun proof-server-send-to-prover (s &optional needs-formatting)
  (message (format "proof-server-send-to-prover: %s" s))
  (when (and s (not (string-equal s "")))
    '(if needs-formatting (message "needs formatting") (message "no formatting"))
    (and proof-server-send-to-prover-fun 
	 ;; we format here so we see what's sent by emacsy
	 (let ((cmd (if needs-formatting (proof-server-format-command s) s)))
	   (funcall proof-server-send-to-prover-fun cmd)))))

;;;###autoload
(defun proof-server-process-response (resp)
  (and proof-server-process-response-fun
       (funcall proof-server-process-response-fun resp)))

(provide 'proof-resolver)
;;; proof-resolver.el ends here
