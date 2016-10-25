;;; proof-resolver.el --- dispatch to functions stored in variables
;;
;;; Code:

(require 'pg-vars)
(require 'proof-config)

(defun proof-server-log (src str)
  (with-current-buffer proof-server-log-buffer
    (setq buffer-read-only nil)
    (goto-char (point-max))
    (insert "*" src ":\n")
    (insert str "\n")
    (setq buffer-read-only t)))

;;;###autoload
(defun proof-ready-prover (&optional queuemode)
  (funcall proof-ready-prover-fun queuemode))

;;;###autoload
(defun proof-invisible-command (cmd)
  (funcall proof-invisible-command-fun cmd))

;;;###autoload
(defun proof-invisible-cmd-handle-result (cmd handler)
  (funcall proof-invisible-cmd-handle-result-fun cmd handler))

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

(defun proof-server-response-complete ()
  (and proof-server-response-complete-fun
       (funcall proof-server-response-complete-fun)))

(defun proof-server-response-incomplete ()
  (and proof-server-response-complete-fun
       (not (funcall proof-server-response-complete-fun))))

;;;###autoload
(defun proof-server-everything-sent ()
  (and proof-everything-sent-fun
       (funcall proof-everything-sent-fun)))

;;;###autoload
(defun proof-server-send-to-prover (string-or-fun &optional special-processor)
  (when (and string-or-fun (not (and (stringp string-or-fun) (string-equal string-or-fun ""))))
    (and proof-server-send-to-prover-fun 
	 (let ((inhibit-quit t))
	   (funcall proof-server-send-to-prover-fun string-or-fun special-processor)))))

;;;###autoload
(defun proof-server-process-response (resp)
  (and proof-server-process-response-fun
       (funcall proof-server-process-response-fun resp)))

(provide 'proof-resolver)
;;; proof-resolver.el ends here
