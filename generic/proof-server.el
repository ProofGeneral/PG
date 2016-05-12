;; ;;; proof-server.el --- Proof General server 
;;
;;; Code:

;;;###autoload
(defun proof-server-ready-prover (queuemode)
  (message "Called proof-server-ready-prover")
  t)

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

(provide 'proof-server)

;;; proof-server.el ends here
