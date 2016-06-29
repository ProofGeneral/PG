;; coq-server.el -- code related to server mode for Coq in Proof General

(require 'xml)
(require 'proof-queue)
(require 'proof-server)
(require 'proof-script)
(require 'pg-goals)
(require 'coq-response)
(require 'coq-stateinfo)
(require 'coq-xml)
(require 'cl-lib)

(defvar coq-server--protocol-buffer-name "*coq-protocol-debug*")
(defvar coq-server-protocol-buffer (get-buffer-create coq-server--protocol-buffer-name))

(defvar coq-server-init (coq-xml-call '((val . Init)) (coq-xml-option '((val . none)))))

(defvar coq-server-pending-state-id nil)

;; leading space makes buffer invisible, for the most part
(defvar coq-server--response-buffer-name " *coq-responses*")
(defvar coq-server-response-buffer (get-buffer-create coq-server--response-buffer-name))

(defvar coq-server--pending-response nil)

;; buffer for responses from coqtop process, nothing to do with user's response buffer
(defun coq-server--append-response (s)
  (with-current-buffer coq-server-response-buffer
    (goto-char (point-max))
    (insert s)))

(defun coq-server--unescape-string (s)
  (replace-regexp-in-string "&nbsp;" " " s))

;; XML parser does not understand &nbsp;
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

(defun coq-server-ready-to-send ()
  (null coq-server--pending-response))

;; if we haven't gotten a response, sleep to allow process filter to receive data
(defun coq-server-wait-until-ready-to-send ()
  (accept-process-output))

;; simplified version of proof shell code for handling errors
(defun coq-server--handle-error ()
  "Take action on errors."
  ;; TODO beep?
  (with-current-buffer 
      proof-script-buffer
    (proof-with-current-buffer-if-exists 
     proof-script-buffer
     (save-excursion
       (proof-script-clear-queue-spans-on-error proof-last-span nil)))
    (setq proof-action-list nil)))

(defun coq-server--clear-response-buffer ()
  (coq--display-response "")
  (pg-response-clear-displays))

;; clear response buffer when we Add an item from the Coq script
(add-hook 'proof-server-insert-hook 'coq-server--clear-response-buffer)

;; send data to Coq by sending to process
;; called by proof-server-send-to-prover
;; do not call directly
(defun coq-server-send-to-prover (s)
  ;; wait until we have the last response we need
  (while (not (coq-server-ready-to-send))
    (coq-server-wait-until-ready-to-send))
  (message "setting pending response")
  (setq coq-server--pending-response t)
  ;; send actual data
  (process-send-string proof-server-process s)
  ;; newline to force response
  (process-send-string proof-server-process "\n"))

;; Unicode!
(defvar impl-bar-char ?―)

;;; goal formatting

(defun coq-server--goal-id (goal)
  (coq-xml-body1 (nth 2 goal)))

(defun coq-server--goal-hypotheses (goal)
  (coq-xml-body (nth 3 goal)))

(defun coq-server--goal-goal (goal)
  (coq-xml-body1 (nth 4 goal)))

(defvar goal-indent " ")

;; make a pretty goal 
(defun coq-server--format-goal-with-hypotheses (goal hyps)
  (let* ((nl "\n")
	 (nl-indent (concat nl goal-indent))
	 (min-width 5)   ; minimum width of implication bar
	 (padding-len 1) ; on either side of hypotheses or goals
	 (padding (make-string padding-len ?\s))
	 (hyps-text (mapcar 'coq-xml-body1 hyps))
	 (formatted-hyps (mapconcat 'identity hyps-text (concat nl-indent padding)))
	 (hyps-width (apply 'max (cons 0 (mapcar 'length hyps-text)))) ; cons 0 in case empty
	 (goal-width (length goal))
	 (width (max min-width (+ (max hyps-width goal-width) (* 2 padding-len))))
	 (goal-offset (/ (- width goal-width) 2)))
    (concat goal-indent padding formatted-hyps nl             ; hypotheses
	    goal-indent (make-string width impl-bar-char) nl  ; implication bar
            goal-indent (make-string goal-offset ?\s) goal))) ; the goal

(defun coq-server--format-goal-no-hypotheses (goal)
  (concat goal-indent goal))

;; invariant: goals is non-empty
(defun coq-server--display-goals (goals)
  (let* ((num-goals (length goals))
	 (goal1 (car goals))
	 (goals-rest (cdr goals))
	 (goal-counter 1))
    (with-temp-buffer
      (if (eq num-goals 1)
	  (insert "1 subgoal")
	(insert (format "%d subgoals" num-goals)))
      (insert "\n\n")
      (insert (format "subgoal 1 (ID %s):\n" (coq-server--goal-id goal1)))
      (insert (coq-server--format-goal-with-hypotheses 
	       (coq-server--goal-goal goal1)
	       (coq-server--goal-hypotheses goal1)))
      (insert "\n")
      (dolist (goal goals-rest)
	(setq goal-counter (1+ goal-counter))
	(insert (format "subgoal %s (ID %s):\n" goal-counter (coq-server--goal-id goal)))
	(insert (coq-server--format-goal-no-hypotheses 
		 (coq-server--goal-goal goal))))
      (pg-goals-display (buffer-string) t))))

;; update global state in response to status
(defun coq-server--handle-status (maybe-current-proof all-proofs current-proof-id)
  (message (format "maybe-current-proof: %s\nall-proofs %s\ncurrent-proof-id: %s"
		   maybe-current-proof all-proofs current-proof-id))
  (let ((curr-proof-opt-val (coq-xml-attr-value maybe-current-proof 'val)))
    (if (string-equal curr-proof-opt-val 'some)
      (let* ((curr-proof-string (coq-xml-body1 maybe-current-proof))
	     (curr-proof-name (coq-xml-body1 curr-proof-string)))
	(setq coq-current-proof-name curr-proof-name))
      (setq coq-current-proof-name nil)))
  (let* ((pending-proof-strings (coq-xml-body all-proofs))
	 (pending-proofs (mapcar 'coq-xml-body1 pending-proof-strings)))
    (setq coq-pending-proofs pending-proofs))
  (let* ((proof-state-id-string (coq-xml-body1 current-proof-id))
	 (proof-state-id (string-to-number proof-state-id-string)))
    (setq coq-proof-state-id proof-state-id))
  ;; used to be called as a hook at end of proof-done-advancing
  (coq-set-state-infos))

	      
(defun coq-server--handle-item (item in-good-value level) 
  ;; in-good-value means, are we looking at a subterm of a value response
  ;; level is indentation for logging
  '(message (format "coq-server--handle-item: %s %s %s" item in-good-value level))
  (insert (make-string level ?\s))
  ;; value tags with fail contain an untagged string in the body, probably a bug
  (pcase (or (stringp item) (coq-xml-tag item))
    (`unit (insert (format "unit\n")))
    (`union (insert (format "union, %s:\n" (coq-xml-attr-value item 'val)))
	    (dolist (body-item (coq-xml-body item))
	      (coq-server--handle-item body-item in-good-value (+ level 2))))
    (`string (insert (format "string: %s\n" (coq-xml-body1 item))))
    (`loc_s (insert (format "start location: %s\n" (coq-xml-attr-value item 'loc_s))))
    (`loc_e (insert (format "end location: %s\n" (coq-xml-attr-value item 'loc_e))))
    (`state_id 
     (let* ((state-id (coq-xml-attr-value item 'val)))
       (insert (format "state_id: %s\n" state-id))
       (when in-good-value
	 (setq coq-current-state-id state-id) ; update global state
	 ;; if there are no more Adds to do, get goal and status
	 (if (null (cdr proof-action-list))
	     (progn
	       (proof-server-send-to-prover (coq-xml-goal))
	       (proof-server-send-to-prover (coq-xml-status)))
	   (message (format "proof-action-list: %s" proof-action-list))))))
    (`status 
     (let* ((status-items (coq-xml-body item))
	    ;; ignoring module path of proof
	    (maybe-current-proof (nth 1 status-items))
	    (all-proofs (nth 2 status-items))
	    (current-proof-id (nth 3 status-items)))
       (coq-server--handle-status maybe-current-proof all-proofs current-proof-id)))
    (`option 
     (let ((val (coq-xml-attr-value item 'val)))
       (insert (format "option: %s\n" val))
       (when (string-equal val 'some)
	 (let ((children (xml-node-children item)))
	   (dolist (child children)
	     (coq-server--handle-item child in-good-value (+ level 1)))))))
    (`goals
     (let* ((children (xml-node-children item))
	    (current-goals (coq-xml-body (nth 0 children)))
	    (bg-goals (coq-xml-body (nth 1 children)))
	    (shelved-goals (coq-xml-body (nth 2 children)))
	    (abandoned-goals (coq-xml-body (nth 3 children))))
       (if current-goals
	   (progn
	     (insert "goals:\n")
	     (dolist (goal current-goals)
	       (coq-server--handle-item goal in-good-value (+ level 1)))
	     (coq-server--display-goals current-goals))
	 (progn
	   (insert "<no goals>")
	   (setq proof-prover-proof-completed 0)
	   ;; clear goals display
	   (pg-goals-display "" nil)
	   ;; mimic the coqtop REPL, though it would be better to come via XML
	   (coq--display-response "No more subgoals.")))
       (insert (make-string level ?\s))
       (if bg-goals
	   (progn
	     (insert "background goals:\n")
	     (dolist (goal bg-goals)
	       (coq-server--handle-item goal in-good-value (+ level 1))))
	 (insert "<no background goals>\n"))
       (insert (make-string level ?\s))
       (if shelved-goals
	   (progn
	     (insert "shelved goals:\n")
	     (dolist (goal shelved-goals)
	       (coq-server--handle-item goal in-good-value (+ level 1))))
	 (insert "<no shelved goals>\n"))
       (insert (make-string level ?\s))
       (if abandoned-goals
	   (progn
	     (insert "abandoned goals:\n")
	     (dolist (goal abandoned-goals)
	       (coq-server--handle-item goal in-good-value (+ level 1))))
	 (insert "<no abandoned goals>\n"))))
    (`goal
     (let* ((children (xml-node-children item))
	    (goal-number (coq-xml-body1 (nth 0 children)))
	    (goal-hypotheses (coq-xml-body (nth 1 children)))
	    (goal (coq-xml-body1 (nth 2 children))))
       (insert (format "goal %s\n" goal-number))
       (insert (make-string level ?\s))
       (if goal-hypotheses
	   (progn
	     (insert "hypotheses:\n")
	     (coq-server--handle-item goal-hypotheses in-good-value (+ level 1)))
	 (insert "<no hypotheses>\n"))
       (insert (make-string level ?\s))
       (insert (format "goal: %s\n" goal))))
    (`pair 
     (dolist (item-child (xml-node-children item))
       (coq-server--handle-item item-child in-good-value (+ level 1))))
    (`list 
     (dolist (item-child (xml-node-children item))
       (coq-server--handle-item item-child in-good-value (+ level 1))))
    (default
      (insert (format "untagged item: %s\n" item)))))

(defun coq-server--handle-feedback (xml)
  '(message (format "got feedback: %s" xml))
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
		 (coq-server--handle-item feedback-child nil 1)))
	     (when (string-equal feedback-value 'errormsg)
	       (let* ((body (coq-xml-body child))
		      (loc (nth 0 body))
		      (loc-start (string-to-number (coq-xml-attr-value loc 'start)))
		      (loc-stop (string-to-number (coq-xml-attr-value loc 'stop)))
		      (message-str (nth 1 body))
		      (message (coq-xml-body1 message-str)))
		 (pg-response-clear-displays)
		 (coq--highlight-error loc-start (- loc-stop loc-start))
		 (coq--display-response message)))))
	  (default
	    (coq-server--handle-item child nil 1)))))))

(defun coq-server--handle-message (xml)
  '(message (format "got message: %s" xml))
  (with-current-buffer coq-server-protocol-buffer
    (insert "*Message:\n")
    (dolist (child (xml-node-children xml))
      (pcase (coq-xml-tag child)
	(`message_level
	 (let ((level (coq-xml-attr-value child 'val)))
	   (insert (format " Level: %s\n" level))))
	(`string
	 (let ((message (coq-server--unescape-string (coq-xml-body1 child))))
	   (insert (format " Message: %s\n" message))
	   (coq--display-response message)))
	(default
	  (coq-server--handle-item xml nil 1))))))

(defun coq-server--handle-value (xml)
  '(message (format "got value: %s" xml))
  (message "setting pending reponse to nil")
  (setq coq-server--pending-response nil)
  (with-current-buffer coq-server-protocol-buffer
    (insert "*Value:\n")
    (let ((status (coq-xml-attr-value xml 'val)))
      (pcase status
	("fail"
	 (insert " failure\n")
	 (let ((children (xml-node-children xml)))
	   (dolist (child children)
	     (coq-server--handle-item child nil 1)))
	 (coq-server--handle-error))
	("good"
	 (insert " success\n")
	 ;; TODO maybe it's better to use the feedback message, which contains the state id
	 ;; but conceivable you'd get the feedback, then a failure ?
	 (when coq-server-pending-state-id ; from Edit_at
	   (setq coq-current-state-id coq-server-pending-state-id)
	   (setq coq-server-pending-state-id nil))
	 (let ((children (xml-node-children xml)))
	   (dolist (child children)
	     (coq-server--handle-item child t 1))))
	(default (insert (format "Unknown value status: %s" xml)))))))

;; process XML response from Coq
(defun coq-server-process-response (response)
  '(message "coq-proof-server-process-response: %s" response)
  (coq-server--append-response response)
  (let ((xml (coq-server--get-next-xml)))
    (while xml
      '(message (format "xml: %s" xml))
      (pcase (coq-xml-tag xml)
	(`value (coq-server--handle-value xml))
	(`feedback (coq-server--handle-feedback xml))
	(`message (coq-server--handle-message xml))
	(default (message "unknown response %s" xml)))
      (setq xml (coq-server--get-next-xml)))))

(provide 'coq-server)
