;;; coq-server.el -- code related to server mode for Coq in Proof General

(require 'xml)
(require 'proof-queue)
(require 'proof-server)
(require 'coq-xml)

(defvar coq-server--protocol-buffer-name "*coq-protocol*")
(defvar coq-server-protocol-buffer (get-buffer-create coq-server--protocol-buffer-name))

(defvar coq-server-init (coq-xml-call '((val . Init)) (coq-xml-option '((val . none)))))

; leading space makes buffer invisible, for the most part
(defvar coq-server--response-buffer-name " *coq-responses*")
(defvar coq-server-response-buffer (get-buffer-create coq-server--response-buffer-name))

(defun coq-server--append-response (s)
  (with-current-buffer coq-server-response-buffer
    (goto-char (point-max))
    (insert s)))

(defun coq-server--unescape-string (s)
  (replace-regexp-in-string "&nbsp;" " " s))

; XML parser does not understand &nbsp;
(defun coq-server--unescape-buffer () 
  (let ((contents (buffer-string)))
    (erase-buffer)
    (insert (coq-server--unescape-string contents))
    (goto-char (point-min))))

(defun coq-server--get-next-xml ()
  (ignore-errors ; returns nil if no XML available
    (with-current-buffer coq-server-response-buffer
      (goto-char (point-min))
      (coq-server--unescape-buffer)
      (let ((xml (xml-parse-tag-1)))
	(when xml
	  (delete-region (point-min) (point)))
	xml))))
    
; list of state_ids for Add calls for which we'll need to send a Goal call, if successful
(defvar coq-server--pending-add-table
  (make-hash-table :size 100 :test 'equal))

(defun coq-server-make-add-pending (item)
  (message "called coq-server-make-add-pending on: %s" item)
  (when (and (coq-xml-tagp item 'call)
	     (string-equal (coq-xml-attr-value item 'val) 'Add))
    (let* ((outer-pair (car (xml-node-children item)))
	   (outer-pair-children (xml-node-children outer-pair))
	   (pair-with-state_id (nth 2 outer-pair-children))
	   (state_id-item (car (xml-node-children pair-with-state_id)))
	   (state-id (coq-xml-attr-value state_id-item 'val))
	   (next-state-id (number-to-string (+ (string-to-number state-id) 1))))
      (message "state id: %s" state-id)
      (puthash next-state-id t coq-server--pending-add-table)
      (message "pending adds")
      (maphash 
       (lambda (k v) (message "add: %s" k))
       coq-server--pending-add-table))))

; send data to Coq by sending to process
(defun coq-server-send-to-prover (s)
  (message "called coq-server-send-to-prover")
  (when s
    (let ((xml (coq-xml-string-to-xml s)))
      (coq-server-make-add-pending xml))
    (process-send-string proof-server-process s)
    ; newline to force response
    (process-send-string proof-server-process "\n")))

(defun coq-server--handle-item (item in-value level) ; level is indentation
  (insert (make-string level ?\s))
  (pcase (coq-xml-tag item)
    (`unit (insert (format "unit\n")))
    (`union (insert (format "union, %s:\n" (coq-xml-attr-value item 'val)))
	    (coq-server--handle-item (coq-xml-body item) in-value (+ level 2)))
    (`string (insert (format "string: %s\n" (coq-xml-body item))))
    (`loc_s (insert (format "start location: %s\n" (coq-xml-attr-value item 'loc_s))))
    (`loc_e (insert (format "end location: %s\n" (coq-xml-attr-value item 'loc_e))))
    (`state_id 
     (let* ((state-id (coq-xml-attr-value item 'val)))
       (insert (format "state_id: %s\n" state-id))
       (when in-value
	 (let ((pendingp (gethash state-id coq-server--pending-add-table)))
	   (message (format "for state-id: %s" state-id))
	   (message "pending? %s" pendingp)
	   (maphash 
	    (lambda (k v) (message "pending: %s" k))
	    coq-server--pending-add-table)
	   (when pendingp
	     ; TODO remove state-id from hash table
	     (coq-server-send-to-prover (coq-xml-goal)))))))
    (`option 
     (let ((val (coq-xml-attr-value item 'val)))
       (insert (format "option: %s\n" val))))
    (`pair 
     (dolist (item-child (xml-node-children item))
       (coq-server--handle-item item-child in-value (+ level 1))))
    (`list 
     (dolist (item-child (xml-node-children item))
       (coq-server--handle-item item-child in-value (+ level 1))))
    (default
      (insert (format "item: %s\n" item)))))

(defun coq-server--handle-feedback (xml)
  (message (format "got feedback: %s" xml))
  (let* ((object (coq-xml-attr-value xml 'object))
	 (route (coq-xml-attr-value xml 'route))
	 (children (xml-node-children xml))) ; state_id, feedback_content 
    (with-current-buffer coq-server-protocol-buffer
      (insert "*Feedback:\n")
      (insert (format " object: %s  route: %s\n" object route))
      (dolist (child children)
	(pcase (coq-xml-tag child)
	  (`feedback_content 
	   (let ((feedback-value (coq-xml-attr-value child 'val)))
	     (insert (format " content value: %s\n" feedback-value))
	     (let ((feedback-children (xml-node-children child)))
	       (dolist (feedback-child feedback-children)
		 (coq-server--handle-item feedback-child nil 1)))))
	  (default
	    (coq-server--handle-item child nil 1)))))))

(defun coq-server--handle-message (xml)
  (message (format "got message: %s" xml))
  (with-current-buffer coq-server-protocol-buffer
    (insert "*Message:\n")
    (dolist (child (xml-node-children xml))
      (pcase (coq-xml-tag child)
	(`message_level
	 (let ((level (coq-xml-attr-value child 'val)))
	   (insert (format " Level: %s\n" level))))
	(`string
	 (let ((message (coq-server--unescape-string (coq-xml-body child))))
	   (insert (format "Message: %s\n" message))
	   (pg-response-display message)))
	(default
	  (coq-server--handle-item xml nil 1))))))

(defun coq-server--handle-value (xml)
  (message (format "got value: %s" xml))
  (with-current-buffer coq-server-protocol-buffer
    (insert "*Status:\n")
    (let ((status (coq-xml-attr-value xml 'val)))
      (pcase status
	("fail"
	 (insert " failure\n")
	 (let ((children (xml-node-children xml)))
	   (dolist (child children)
	     (coq-server--handle-item child t 1))))
	("good"
	 (insert " success\n")
	 (let ((children (xml-node-children xml)))
	   (dolist (child children)
	     (coq-server--handle-item child t 1))))
	(default (insert (format "Unknown value status: %s" xml)))))))

; process XML response from Coq
(defun coq-server-process-response (response)
  (message "coq-proof-server-process-response: %s" response)
  (coq-server--append-response response)
  (let ((xml (coq-server--get-next-xml)))
    (while xml
      (message (format "xml: %s" xml))
      (pcase (coq-xml-tag xml)
	(`value (coq-server--handle-value xml))
	(`feedback (coq-server--handle-feedback xml))
	(`message (coq-server--handle-message xml))
	(default (message "unknown response %s" xml)))
      (setq xml (coq-server--get-next-xml)))))
	
(provide 'coq-server)

