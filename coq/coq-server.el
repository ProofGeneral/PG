;;; coq-server.el -- code related to server mode for Coq in Proof General

(require 'xml)
(require 'proof-queue)
(require 'proof-server)
(require 'coq-xml)

(defvar coq-server-init (coq-xml-call '((val . Init)) (coq-xml-option '((val . none)))))

; check value of a value response, 
(defun coq-server-successful-response (response)
  (and (coq-xml-tagp response 'value)
       (let ((val (coq-xml-attr-value response 'val)))
	 (eq val 'good))))

; send data to Coq by sending to process
(defun coq-server-send-to-prover (cmd)
  (message "called coq-server-send-to-prover")
  (when cmd
    (process-send-string proof-server-process cmd)
    ; newline to force response
    (process-send-string proof-server-process "\n")))

; process XML response from Coq
(defun coq-server-process-response (response)
  (message "coq-proof-server-process-response")
  ; might get several XML trees here, which will flummox the XML parser
  ; so wrap response in dummy tags
  (let* ((tagged-response (coq-xml-block "dummytag" '() (list response)))
	 (parsed-xml
	  (with-temp-buffer
	    (insert tagged-response)
	    (xml-parse-region (point-min) (point-max))))
	 (xml (car parsed-xml)))
    (message "xml: %s" xml)
    (dolist (child (xml-node-children xml))
      (message "child: %s" child))))
      


(provide 'coq-server)

