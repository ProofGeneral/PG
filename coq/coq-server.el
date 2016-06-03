;;; coq-server.el -- code related to server mode for Coq in Proof General

(require 'xml)
(require 'proof-queue)
(require 'proof-server)
(require 'coq-xml)

(defvar coq-server-init (coq-xml-call '((val . Init)) (coq-xml-option '((val . none)))))

; list of state_ids for Add calls for which we'll need to send a Goal call, if successful
(defvar coq-server-pending-adds nil)

; check value of a value response, 
(defun coq-server-successful-response (response-xml)
  (and (coq-xml-tagp response-xml 'value)
       (let ((val (coq-xml-attr-value response-xml 'val)))
	 (string-equal val "good"))))

; state_id can appear as the only child in a value
; or as the first element of a pair
(defun coq-server-response-state_id (response-xml)
  (and (coq-server-successful-response response-xml)
       (let* ((nodes (xml-node-children response-xml))
	      (child1 (car nodes)))
	 (message "child1: %s" child1)
	 (or (and (coq-xml-tagp child1 'state_id)
		  (coq-xml-attr-value child1 'val))
	     (and (coq-xml-tagp child1 'pair)
		  (let ((maybe-state_id (car (xml-node-children child1))))
		    (and (coq-xml-tagp maybe-state_id 'state_id)
			 (coq-xml-attr-value maybe-state_id 'val))))))))

(defun coq-server-make-add-pending (item)
  (message "called coq-server-make-add-pending on: %s" item)
  (when (and (coq-xml-tagp item 'call)
	     (string-equal (coq-xml-attr-value item 'val) "Add"))
    (let* ((outer-pair (car (xml-node-children item)))
	   (outer-pair-children (xml-node-children outer-pair))
	   (pair-with-state_id (nth 2 outer-pair-children))
	   (state_id-item (car (xml-node-children pair-with-state_id)))
	   (state_id (coq-xml-attr-value state_id-item 'val)))
      (message "state id: %s" state_id)
      (setq coq-server-pending-adds (cons state_id coq-server-pending-adds))
      (message "pending adds")
      (dolist (add coq-server-pending-adds)
	(message "add: %s" add)))))

; send data to Coq by sending to process
(defun coq-server-send-to-prover (s)
  (message "called coq-server-send-to-prover")
  (when s
    (let ((xml (coq-xml-string-to-xml s)))
      (coq-server-make-add-pending xml))
    (process-send-string proof-server-process s)
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
      (message "child: %s" child)
      (let ((state_id (coq-server-response-state_id child)))
	(message (format "state_id: %s" state_id))
	(when state_id
	  (message "in response: got state_id %s" state_id)
	  ;;; START HERE
	  ;;; if you see a state_id in pending list, send Goal call
)))))

(provide 'coq-server)

