;;; coq-server.el -- code related to server mode for Coq in Proof General

(require 'xml)
(require 'proof-queue)
(require 'coq-xml)

(defvar coq-server-init (coq-xml-call '((val . Init)) (coq-xml-option '((val . none)))))

; check value of a value response, 
(defun successful-response (response)
  (and (coq-xml-tagp response 'value)
       (let ((val (coq-xml-attr-value response 'val)))
	 (eq val 'good))))

; process XML response from Coq
(defun coq-server-process-response (response)
  (message "coq-proof-server-process-response")
  (let* ((parsed-xml
	  (with-temp-buffer
	    (insert response)
	    (xml-parse-region (point-min) (point-max))))
	 (xml (car parsed-xml)))
    (message "xml:")
    (message (format "%s" xml))

))




(provide 'coq-server)

