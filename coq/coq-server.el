;;; coq-server.el -- code related to server mode for Coq in Proof General

(require 'coq-xml)

(defvar coq-server-init (coq-xml-call '((val . Init)) (coq-xml-option '((val . none)))))

(defun coq-server-filter (process response)
  (message "Called server filter")
  (message (concat "Message is " response)))

(provide 'coq-server)

