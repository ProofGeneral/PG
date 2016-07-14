;;; -*- lexical-binding: t -*-

;; coq-server.el -- code related to server mode for Coq in Proof General

(require 'xml)
(require 'thingatpt)

(require 'coq-tq)
(require 'proof-queue)
(require 'proof-server)
(require 'proof-script)
(require 'pg-goals)
(require 'coq-response)
(require 'coq-stateinfo)
(require 'coq-xml)
(require 'cl-lib)

(eval-when-compile 
  (require 'cl))

(defvar coq-server--pending-edit-at-state-id nil
  "State id for an Edit_at command sent, until we get a response.")

(defvar coq-server--start-of-focus-state-id nil
  "When re-opening a proof, this is the state id of the current focus.
If we undo to point before the span with this state id, the focus 
is gone and we have to close the secondary locked span."
  )

(defvar coq-server--end-of-focus-state-id nil
  "When re-opening a proof, this is the state id of the end of 
the current focus."
  )

(defvar coq-server--last-tip-state-id nil
  "When re-opening a proof, this is the state id of the tip before re-opening."
  )

(defvar coq-server--current-span nil
  "Span associated with last response")

;; buffer for gluing coqtop responses into XML
;; leading space makes buffer invisible, for the most part
(defvar coq-server--response-buffer-name " *coq-responses*")
(defvar coq-server-response-buffer (get-buffer-create coq-server--response-buffer-name))

(defvar coq-server-transaction-queue nil)

(defvar end-of-response-regexp "</value>")

;; delay creating the XML so it will have the right state-id
;; the returned lambda captures the passed item, which is why 
;; this file needs lexical binding
(defun coq-server-make-add-command-thunk (cmd span)
  (lambda () 
    (list (coq-xml-add-item cmd) span)))

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
      (let ((xml (xml-parse-tag-1)))
	(when xml
	  (delete-region (point-min) (point)))
	xml))))

(defun coq-server--handle-error ()
  "Take action on errors."
  ;; TODO beep?

  ;; proof shell would clear queue spans, proof action list
  ;; here, we can continue on because of async processing
  ;; script errors don't actually produce a failure response 
  ;; instead, the error messages show up in feedbacks
  ;; and a subsequent Status will give a failure

  ;; so nothing to do here, really ???
  )

(defun coq-server--clear-response-buffer ()
  (coq--display-response "")
  (pg-response-clear-displays))

(defun coq-server--clear-goals-buffer ()
  (pg-goals-display "" nil))

(defun coq-server-start-transaction-queue ()
  (setq coq-server-transaction-queue (tq-create proof-server-process)))

;; clear response buffer when we Add an item from the Coq script
(add-hook 'proof-server-insert-hook 'coq-server--clear-response-buffer)
;; start transaction queue after coqtop process started
(add-hook 'proof-server-init-hook 'coq-server-start-transaction-queue)

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
      (insert "\n\n")
      (dolist (goal goals-rest)
	(setq goal-counter (1+ goal-counter))
	(insert (format "\nsubgoal %s (ID %s):\n" goal-counter (coq-server--goal-id goal)))
	(insert (coq-server--format-goal-no-hypotheses 
		 (coq-server--goal-goal goal))))
      (pg-goals-display (buffer-string) t))))

;; update global state in response to status
(defun coq-server--handle-status (maybe-current-proof all-proofs current-proof-id)
  (let ((curr-proof-opt-val (coq-xml-val maybe-current-proof)))
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
  
  ;; value tags with fail contain an untagged string in the body, probably a bug
  (pcase (or (stringp item) (coq-xml-tag item))
    (`unit )
    (`union 
     (dolist (body-item (coq-xml-body item))
       (coq-server--handle-item body-item in-good-value (+ level 2))))
    (`string )
    (`loc_s )
    (`loc_e )
    (`state_id 
     (let* ((state-id (coq-xml-val item)))
       ;; deal with edit-at case separately
       ;; state ids there are not the current state id
       (when (and in-good-value (not coq-server--pending-edit-at-state-id))
	 (message "1> setting current state id: %s" state-id)
	 (setq coq-current-state-id state-id) ; update global state
	 (when coq-server--current-span
	   (coq-set-span-state-id coq-server--current-span state-id))
	 (unless coq-retract-buffer-state-id  ; happens once per buffer
	   (setq coq-retract-buffer-state-id state-id))
	 ;; if there are no more Adds to do, get goal and status
	 (if (null (cdr proof-action-list))
	     (progn
	       (proof-server-send-to-prover (coq-xml-goal))
	       (proof-server-send-to-prover (coq-xml-status)))))))
    (`status 
     (let* ((status-items (coq-xml-body item))
	    ;; ignoring module path of proof
	    (maybe-current-proof (nth 1 status-items))
	    (all-proofs (nth 2 status-items))
	    (current-proof-id (nth 3 status-items)))
       (coq-server--handle-status maybe-current-proof all-proofs current-proof-id)))
    (`option 
     (let ((val (coq-xml-val item)))
       
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
	     (dolist (goal current-goals)
	       (coq-server--handle-item goal in-good-value (+ level 1)))
	     (coq-server--display-goals current-goals))
	 (progn
	   (setq proof-prover-proof-completed 0)
	   ;; clear goals display
	   (coq-server--clear-goals-buffer)
	   ;; mimic the coqtop REPL, though it would be better to come via XML
	   (coq--display-response "No more subgoals.")))
       (when bg-goals
	 (dolist (goal bg-goals)
	   (coq-server--handle-item goal in-good-value (+ level 1))))
       (when shelved-goals
	 (dolist (goal shelved-goals)
	   (coq-server--handle-item goal in-good-value (+ level 1))))
       (when abandoned-goals
	 (dolist (goal abandoned-goals)
	   (coq-server--handle-item goal in-good-value (+ level 1))))))
    (`goal
     (let* ((children (xml-node-children item))
	    (goal-number (coq-xml-body1 (nth 0 children)))
	    (goal-hypotheses (coq-xml-body (nth 1 children)))
	    (goal (coq-xml-body1 (nth 2 children))))
       
       
       (if goal-hypotheses
	   (progn
	     
	     (coq-server--handle-item goal-hypotheses in-good-value (+ level 1)))
	 )
       
       ))
    (`pair 
     (dolist (item-child (xml-node-children item))
       (coq-server--handle-item item-child in-good-value (+ level 1))))
    (`list 
     (dolist (item-child (xml-node-children item))
       (coq-server--handle-item item-child in-good-value (+ level 1))))
    (default
      )))

;; similar to next-span in lib/span.el
;; but looks at point after 
(defun coq-server--next-span (span)
  (let ((end (span-end span)))
    (if (< end (point-max))
	(let* ((pt-after (1+ end))
	       (all-spans (overlays-in pt-after pt-after))
	       (sorted-spans (sort (lambda (sp1 sp2) (< (span-start sp1) (span-start sp2))) all-spans)))
	  (car-safe sorted-spans))
      (error "Looking for non-existent span after focus"))))

;; inefficient, but number of spans should be small
(defun coq-server--state-id-precedes (state-id-1 state-id-2)
  "Does STATE-ID-1 occur in a span before that for STATE-ID-2?"
  (let ((span1 (coq-server--find-span-with-state-id state-id-1))
	(span2 (coq-server--find-span-with-state-id state-id-2)))
    (< (span-start span1) (span-start span2))))

(defun coq-server--find-span-with-predicate (pred &optional span-list)
  (with-current-buffer proof-script-buffer
    (let* ((all-spans (or span-list (overlays-in (point-min) (point-max)))))
      (cl-find-if pred all-spans))))

(defun coq-server--find-span-with-state-id (state-id &optional span-list)
  (coq-server--find-span-with-predicate
   (lambda (span) 
     (equal (span-property span 'state-id) state-id))
   span-list))

;; is error span at end of locked region
;; use as coloring heuristic 
(defun coq-server--error-span-at-end-of-locked (error-span)
  ;; TODO, we should be using proof-locked-span
  ;; but that seems to be nil
  (let* ((locked-span (coq-server--find-span-with-predicate
		       (lambda (span) 
			 (equal (span-property span 'face) 'proof-locked-face))))
	 (locked-end (span-end locked-span))
	 (error-end (span-end error-span)))
    '(message "error end: %s  locked-end: %s" error-end locked-end)
    (= error-end locked-end)))

(defun coq-server--handle-feedback (xml)
  '(message (format "got feedback: %s" xml))
  (let* ((object (coq-xml-attr-value xml 'object))
	 (route (coq-xml-attr-value xml 'route))
	 (children (xml-node-children xml)) ; state_id, feedback_content 
	 in-error
	 error-state-id
	 error-start
	 error-stop
	 error-message)
    (dolist (child children)
      (pcase (coq-xml-tag child)
	(`feedback_content 
	 (let ((feedback-value (coq-xml-val child)))
	   
	   (let ((feedback-children (xml-node-children child)))
	     (dolist (feedback-child feedback-children)
	       (coq-server--handle-item feedback-child nil 1)))
	   (when (string-equal feedback-value "errormsg")
	     (setq in-error t)
	     (let* ((body (coq-xml-body child))
		    (loc (nth 0 body))
		    (loc-start (string-to-number (coq-xml-attr-value loc 'start)))
		    (loc-stop (string-to-number (coq-xml-attr-value loc 'stop)))
		    (msg-str (nth 1 body))
		    (msg (coq-xml-body1 msg-str)))
	       (setq error-start loc-start)
	       (setq error-stop loc-stop)
	       (setq error-message msg)
	       (pg-response-clear-displays)
	       (coq--display-response msg)))))
	(`state_id ;; maybe not error, save state id in case
	 (setq error-state-id (coq-xml-val child)))))
    (when in-error
      (let ((error-span (coq-server--find-span-with-state-id error-state-id)))
	;; coloring heuristic
	;; if error is at end of locked span, there's no async involved, do nothing
	;;   we've already given temp coloring for that via coq--highlight-error
	;; if error is in middle, indelibly color the span containing the error 
	;; TODO this will change if we have multiple spans for proofs
	(if (coq-server--error-span-at-end-of-locked error-span)
	    ;; error in last sentence processed
	    (coq--highlight-error error-span error-start error-stop)
	  ;; error in middle of processed region
	  (coq--mark-error error-span error-message))))))

(defun coq-server--handle-message (xml)
  (dolist (child (xml-node-children xml))
    (pcase (coq-xml-tag child)
      (`message_level
       (let ((level (coq-xml-val child)))
	 ))
      (`string
       (let ((message (coq-server--unescape-string (coq-xml-body1 child))))
	 (coq--display-response message)))
      (default
	(coq-server--handle-item xml nil 1)))))

(defun coq-server--value-new-focusp (xml)
  (let ((child1 (coq-xml-body1 xml)))
    (and child1
	 (coq-xml-tagp child1 'union)
	 (string-equal (coq-xml-val child1) "in_r"))))

(defun coq-server--simple-backtrack ()
  ;; delete all spans marked for deletion
  (with-current-buffer proof-script-buffer
    (let ((all-spans (overlays-in (point-min) (point-max))))
      (mapc (lambda (span)
	      (when (span-property span 'marked-for-deletion)
		(span-delete span)))
	    all-spans))))

;; focus-end-state-id represents end of re-opened proof
;; last-tip-state-id represents tip before we re-opened proof
;; want a secondary locked span just past focus end to old tip
;; also, restore the just-deleted spans within that secondary locked region
(defun coq-server--create-secondary-locked-span (focus-end-state-id last-tip-state-id)
  (message "create secondary span, focus-id: %s last tip state id: %s" focus-end-state-id last-tip-state-id)
  ;; proof-script-span-cache is list of (start end property-list), sorted by start
  ;; the last one should be data for last tip, since that's where we retracted from
  ;; so look for focus-end-state-id, restore all spans after that
  (with-current-buffer proof-script-buffer
    (let* ((all-spans (overlays-in (point-min) (point-max)))
	   (marked-spans (cl-remove-if-not 
			  (lambda (span) (span-property span 'marked-for-deletion)) 
			  all-spans))
	   (sorted-marked-spans 
	    (sort marked-spans (lambda (sp1 sp2) (< (span-start sp1) (span-start sp2)))))
	   (last-tip-span (coq-server--find-span-with-state-id last-tip-state-id))
	   found-focus-end
	   secondary-span-start
	   secondary-span-end)
      (message "num span data to check: %s" (length sorted-marked-spans))
      (message "last tip span: %s with properties: %s" last-tip-span (span-properties last-tip-span))
      (setq secondary-span-end (span-end last-tip-span))
      (dolist (span sorted-marked-spans)
	(message "looking at span: %s" span)
	(if found-focus-end
	    ;; restore span, get secondary span bounds
	    (progn
	      (let ((curr-span-start (span-start span))
		    (curr-span-end (span-end span)))
		;; the first span past the end of the focus starts the secondary span
		(unless secondary-span-start 
		  (setq secondary-span-start curr-span-start))
		;; don't delete the span 
		(span-unmark-delete span)))
	  ;; look for focus end
	  (let ((span-state-id (span-property span 'state-id)))
	    (message "looking for focus-end-state-id: %s, found %s" focus-end-state-id span-state-id)
	    (if (and span-state-id (equal span-state-id focus-end-state-id))
		(progn 
		  (setq found-focus-end t)
		  (message "found focus-end-state-id: %s" span-state-id))
	      (span-delete span)))))
      ;; skip past whitespace for secondary span
      (save-excursion
	(goto-char secondary-span-start)
	(while (thing-at-point 'whitespace)
	  (goto-char (1+ (point))))
	(beginning-of-thing 'sentence)
	(setq secondary-span-start (point)))
      (message "making secondary span with start: %s end: %s" secondary-span-start secondary-span-end)
      (let* ((span (span-make secondary-span-start secondary-span-end)))
	(message "made a span")
	(span-set-property span 'start-closed t)
	(span-set-property span 'end-closed t)
	(message "setting span face")
	(span-set-property span 'face 'proof-secondary-locked-face)
	(message "set some properties on it")
	(message "setting span text r/o")
	(put-text-property secondary-span-start secondary-span-end 'read-only t proof-script-buffer)
	(message "here is the locked 2ndary span: %s" span)
	(setq proof-locked-secondary-span span)))))

(defun coq-server--handle-value (xml)
  '(message "Got value: %s" xml)
  (let ((status (coq-xml-val xml)))
    (pcase status
      ("fail"
       (let ((children (xml-node-children xml)))
	 (let ((errmsg (nth 1 children))) ; should be wrapped in string tags, bug 4849
	   (coq-server--clear-response-buffer)
	   (coq--display-response errmsg)))
       (coq-server--handle-error))
      ("good"
       ;; TODO maybe it's better to use the feedback message, which contains the state id
       ;; but conceivable you'd get the feedback, then a failure ?
       (message "coq-server--pending-edit-at-state-id %s" coq-server--pending-edit-at-state-id)
       (when coq-server--pending-edit-at-state-id ; from Edit_at
	 ;; if we just retracted to before a re-opened proof, remove secondary locked span, delete 
	 ;; any spans contained between last end of focus and last tip
	 (message "coq-server--start-of-focus-state-id: %s coq-server--end-of-focus-state-id: %s  coq-server--last-tip-state-id: %s"
		  coq-server--start-of-focus-state-id coq-server--end-of-focus-state-id coq-server--last-tip-state-id)
	 (when (and coq-server--start-of-focus-state-id coq-server--end-of-focus-state-id coq-server--last-tip-state-id
		    (or (equal coq-server--pending-edit-at-state-id coq-retract-buffer-state-id)
			(coq-server--state-id-precedes coq-server--pending-edit-at-state-id coq-server--start-of-focus-state-id)))
	   (message "proof-locked-secondary-span: %s" proof-locked-secondary-span)
	   (when proof-locked-secondary-span ;; should always be true
	     (message "removing secondary span")
	     (let ((start (span-start proof-locked-secondary-span))
		   (end (span-end proof-locked-secondary-span)))
	       ;; delete spans between end of last focus (exclusive) and last tip (inclusive)
	       (with-current-buffer proof-script-buffer
		 (message "removing secondary span")
		 (span-delete proof-locked-secondary-span)
		 (setq proof-locked-secondary-span nil)
		 (message "removing read-only prop")
		 (message "about to remove that prop start: %s end: %s" start end)
		 (setq inhibit-read-only t) ; special trick
		 (remove-list-of-text-properties start end (list 'read-only))
		 (setq inhibit-read-only nil)
		 (message "removed r/o property")
		 (let* ((end-of-focus-span (coq-server--find-span-with-state-id coq-server--end-of-focus-state-id))
			(span-after-focus (coq-server--next-span end-of-focus-span))
			(last-tip-span (coq-server--find-span-with-state-id coq-server--last-tip-state-id))
			(candidate-spans (overlays-in (span-start span-after-focus) (span-start last-tip-span)))
			(relevant-spans 
			 (cl-remove-if-not 
			  (lambda (span) (or (span-property span 'type) (span-property span 'idiom)))
			  candidate-spans)))
		   (mapc 'span-delete relevant-spans)))))
	   (setq coq-server--start-of-focus-state-id nil
		 coq-server--end-of-focus-state-id nil
		 coq-server--last-tip-state-id nil))
	 (message "on edit-at, setting coq-current-state-id to %s" coq-server--pending-edit-at-state-id)
	 (setq coq-current-state-id coq-server--pending-edit-at-state-id)
	 (if (coq-server--value-new-focusp xml)
	     ;; new focus produces secondary locked span, which extends from
	     ;; end of new focus to last tip
	     ;; primary locked span is from start of script to the edit at state id
	     (let* ((union (coq-xml-body1 xml))
		    (outer-pair (coq-xml-body1 union))
		    (focus-start-state-id (coq-xml-attr-value (coq-xml-body1 outer-pair) 'val))
		    (inner-pair (nth 1 (coq-xml-body outer-pair)))
		    (inner-pair-children (coq-xml-body inner-pair))
		    (focus-end-state-id (coq-xml-val (nth 0 inner-pair-children)))
		    (last-tip-state-id (coq-xml-val (nth 1 inner-pair-children))))
	       (setq coq-server--start-of-focus-state-id focus-start-state-id)
	       (setq coq-server--end-of-focus-state-id focus-end-state-id)
	       (setq coq-server--last-tip-state-id last-tip-state-id)
	       (with-current-buffer proof-script-buffer
		 (coq-server--create-secondary-locked-span focus-end-state-id last-tip-state-id)))
	   ;; simple backtrack
	   (coq-server--simple-backtrack)))
       (let ((children (xml-node-children xml)))
	 (dolist (child children)
	   (coq-server--handle-item child t 1)))
       ;; must set after processing children
       (when coq-server--pending-edit-at-state-id
	 (setq coq-server--pending-edit-at-state-id nil)))
      (default )))
  ;; now that we've processed value, ready to send next item
  (proof-server-exec-loop))

;; process XML response from Coq
(defun coq-server-process-response (response span)
  (coq-server--append-response response)
  (with-current-buffer coq-server-response-buffer
    (coq-server--unescape-buffer))
  (setq coq-server--current-span span)
  (let ((xml (coq-server--get-next-xml)))
    (while xml
      (pcase (coq-xml-tag xml)
	(`value (coq-server--handle-value xml))
	(`feedback (coq-server--handle-feedback xml))
	(`message (coq-server--handle-message xml))
	(default (message "unknown response %s" xml)))
      (setq xml (coq-server--get-next-xml)))))

(defun coq-server-handle-tq-response (closure response span)
  (coq-server-process-response response span)
  ;; needed to advance proof-action-list
  (proof-server-manage-output response))

;; send data to Coq by sending to process
;; called by proof-server-send-to-prover
;; do not call directly
(defun coq-server-send-to-prover (s)
  (tq-enqueue coq-server-transaction-queue s end-of-response-regexp
	      ;; "closure" argument, passed to handler below
	      nil 
	      ;; handler gets closure and coqtop response
	      'coq-server-handle-tq-response))

(provide 'coq-server)
