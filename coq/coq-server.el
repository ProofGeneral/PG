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

(defvar coq-server--current-span nil
  "Span associated with last response")

;; buffer for gluing coqtop responses into XML
;; leading space makes buffer invisible, for the most part
(defvar coq-server--response-buffer-name " *coq-responses*")
(defvar coq-server-response-buffer (get-buffer-create coq-server--response-buffer-name))

(defvar coq-server-transaction-queue nil)

(defvar end-of-response-regexp "</value>")

;; buffer for responses from coqtop process, not the *response* buffer seen by user
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
	  '(message "xml: %s footprint: %s" xml (coq-xml-footprint xml))
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

(defun coq-server--handle-item (item)
  (pcase (or (stringp item) (coq-xml-tag item))
    (`unit)
    (`union 
     (dolist (body-item (coq-xml-body item))
       (coq-server--handle-item body-item)))
    (`string)
    (`loc_s)
    (`loc_e)
    (`state_id)
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
	     (coq-server--handle-item child))))))
    (`goals
     (let* ((children (xml-node-children item))
	    (current-goals (coq-xml-body (nth 0 children)))
	    (bg-goals (coq-xml-body (nth 1 children)))
	    (shelved-goals (coq-xml-body (nth 2 children)))
	    (abandoned-goals (coq-xml-body (nth 3 children))))
       (if current-goals
	   (progn
	     (dolist (goal current-goals)
	       (coq-server--handle-item goal))
	     (coq-server--display-goals current-goals))
	 (progn
	   (setq proof-prover-proof-completed 0)
	   ;; clear goals display
	   (coq-server--clear-goals-buffer)
	   ;; mimic the coqtop REPL, though it would be better to come via XML
	   (coq--display-response "No more subgoals.")))
       (when bg-goals
	 (dolist (goal bg-goals)
	   (coq-server--handle-item goal)))
       (when shelved-goals
	 (dolist (goal shelved-goals)
	   (coq-server--handle-item goal)))
       (when abandoned-goals
	 (dolist (goal abandoned-goals)
	   (coq-server--handle-item goal)))))
    (`goal
     (let* ((children (xml-node-children item))
	    (goal-number (coq-xml-body1 (nth 0 children)))
	    (goal-hypotheses (coq-xml-body (nth 1 children)))
	    (goal (coq-xml-body1 (nth 2 children))))
       (when goal-hypotheses
	 (coq-server--handle-item goal-hypotheses))))
    (`pair 
     (dolist (item-child (xml-node-children item))
       (coq-server--handle-item item-child)))
    (`list 
     (dolist (item-child (xml-node-children item))
       (coq-server--handle-item item-child)))
    (default)))

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
  (let* ((locked-span (coq-server--find-span-with-predicate
		       (lambda (span) 
			 (equal (span-property span 'face) 'proof-locked-face))))
	 (locked-end (span-end locked-span))
	 (error-end (span-end error-span)))
    (= error-end locked-end)))

(defvar coq-server--value-simple-backtrack-footprint 
  '(value (union (unit))))

(defun coq-server--value-simple-backtrackp (xml)
  (message "LOOKING FOR SIMPLE BACKTRACK: %s AGAINST: %s" (coq-xml-footprint xml) coq-server--value-simple-backtrack-footprint)
  (equal (coq-xml-footprint xml)
	 coq-server--value-simple-backtrack-footprint))

;; Edit_at, get new focus
(defvar coq-server--value-new-focus-footprint 
  '(value (union (pair (state_id) (pair (state_id) (state_id))))))

(defun coq-server--value-new-focusp (xml)
  (message "LOOKING FOR NEW FOCUS WITH: %s AGAINST: %s" (coq-xml-footprint xml) coq-server--value-new-focus-footprint)
  (and (equal (coq-xml-footprint xml)
	      coq-server--value-new-focus-footprint)
       (string-equal (coq-xml-at-path 
		      xml
		      '(value (union val)))
		     "in_r")))

;; extract state ids from value response after focus open
(defun coq-server--new-focus-state-ids (xml)
  (let* ((outer-pair 
	  (coq-xml-at-path 
	   xml
	   '(value (union (pair)))))
	 (focus-start-state-id 
	  (coq-xml-at-path 
	   outer-pair
	   '(pair (state_id val))))
	 (inner-pair 
	  (coq-xml-at-path 
	   outer-pair
	   '(pair (state_id) (pair))))
	 (focus-end-state-id 
	  (coq-xml-at-path 
	   inner-pair
	   '(pair (state_id val))))
	 (last-tip-state-id 
	  (coq-xml-at-path 
	   inner-pair
	   '(pair (state_id) (state_id val)))))
    (list focus-start-state-id focus-end-state-id last-tip-state-id)))

;; value on Init
(defvar coq-server--value-init-state-id-footprint
  '(value (state_id)))

(defun coq-server--value-init-state-idp (xml)
  (equal (coq-xml-footprint xml) 
	 coq-server--value-init-state-id-footprint))

(defun coq-server--value-init-state-id (xml)
  (coq-xml-at-path 
   xml
   '(value (state_id val))))

;; value when updating state id from an Add
(defvar coq-server--value-new-state-id-footprint
  '(value (pair (state_id) (pair (union (unit)) (string)))))

(defun coq-server--value-new-state-idp (xml)
  (equal (coq-xml-footprint xml) 
	 coq-server--value-new-state-id-footprint))

(defun coq-server--value-new-state-id (xml)
  (coq-xml-at-path 
   xml
   '(value (pair (state_id val)))))

;; Add'ing past end of focus
(defvar coq-server--value-end-focus-footprint 
  '(value (pair (state_id) (pair (union (state_id)) (string)))))

(defun coq-server--value-end-focusp (xml) 
  (message "END FOCUSP XML: %s LOOKING FOR: %s" (coq-xml-footprint xml) coq-server--value-end-focus-footprint)
  (and (equal (coq-xml-footprint xml) coq-server--value-end-focus-footprint)
       (string-equal (coq-xml-at-path 
		      xml 
		      '(value (pair (state_id) (pair (union val))))) 
		     "in_r")))

(defun coq-server--end-focus-qed-state-id (xml)
  (coq-xml-at-path 
   xml 
   '(value (pair (state_id val)))))

(defun coq-server--end-focus-new-tip-state-id (xml)
  (coq-xml-at-path 
   xml 
   '(value (pair (state_id) (pair (union (state_id val)))))))

(defun coq-server--simple-backtrack ()
  ;; delete all spans marked for deletion
  (with-current-buffer proof-script-buffer
    (let ((all-spans (overlays-in (point-min) (point-max))))
      (mapc (lambda (span)
	      (when (span-property span 'marked-for-deletion)
		(span-delete span)))
	    all-spans))))

(defun coq-server--new-focus-backtrack (xml)
  (message "NEW FOCUS")
  ;; new focus produces secondary locked span, which extends from
  ;; end of new focus to last tip
  ;; primary locked span is from start of script to the edit at state id
  ;; want a secondary locked span just past focus end to old tip
  (let* ((state-ids (coq-server--new-focus-state-ids xml))
	 (focus-start-state-id (nth 0 state-ids))
	 (focus-end-state-id (nth 1 state-ids))
	 (last-tip-state-id (nth 2 state-ids)))
    (setq coq-server--start-of-focus-state-id focus-start-state-id)
    (with-current-buffer proof-script-buffer
      (coq-server--create-secondary-locked-span focus-end-state-id last-tip-state-id))))

(defun coq-server--create-secondary-locked-span (focus-end-state-id last-tip-state-id)
  (message "create secondary span, focus-id: %s last tip state id: %s" focus-end-state-id last-tip-state-id)
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
      (setq secondary-span-end (span-end last-tip-span))
      (dolist (span sorted-marked-spans)
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
	    (if (and span-state-id (equal span-state-id focus-end-state-id))
		(setq found-focus-end t)
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
	(span-set-property span 'start-closed t) ;; TODO what are these for?
	(span-set-property span 'end-closed t)
	(span-set-property span 'face 'proof-secondary-locked-face)
	(put-text-property secondary-span-start secondary-span-end 'read-only t proof-script-buffer)
	(setq proof-locked-secondary-span span)))))

(defun coq-server--remove-secondary-locked-span (&optional delete-spans)
  (message "removing secondary span")
  (let ((start (span-start proof-locked-secondary-span))
	(end (span-end proof-locked-secondary-span)))
    ;; delete spans covered by secondary span
    (with-current-buffer proof-script-buffer
      (span-delete proof-locked-secondary-span)
      (setq proof-locked-secondary-span nil)
      (setq inhibit-read-only t) ; special trick
      (remove-list-of-text-properties start end (list 'read-only))
      (setq inhibit-read-only nil)
      ;; don't delete when merging primary, secondary locked regions, spans following primary locked region are valid
      (when delete-spans
	(let* ((candidate-spans (overlays-in start end))
	       (relevant-spans 
		(cl-remove-if-not 
		 (lambda (span) (or (span-property span 'type) (span-property span 'idiom)))
		 candidate-spans)))
	  (mapc 'span-delete relevant-spans))))))

(defun coq-server--merge-locked-spans ()
  (with-current-buffer proof-script-buffer
    (let ((new-end (span-end proof-locked-secondary-span)))
      (coq-server--remove-secondary-locked-span)
      ;; proof-done-advancing uses this to set merged locked end
      (setq proof-merged-locked-end new-end))))

;; did we backtrack to a point before the current focus
(defun coq-server--backtrack-before-focusp ()
  (and coq-server--start-of-focus-state-id 
       (or (equal coq-server--pending-edit-at-state-id coq-retract-buffer-state-id)
	   (coq-server--state-id-precedes 
	    coq-server--pending-edit-at-state-id 
	    coq-server--start-of-focus-state-id))))

(defun coq-server--update-state-id-and-process (state-id)
  (setq coq-current-state-id state-id)
  (when coq-server--current-span
    (coq-set-span-state-id coq-server--current-span state-id))
  ;; no more Adds to do
  (when (coq-server--finished-adds)
    (proof-server-send-to-prover (coq-xml-goal))
    (proof-server-send-to-prover (coq-xml-status)))
  ;; processed good value, ready to send next item
  (proof-server-exec-loop))

(defun coq-server--handle-value (xml)
  (message "GOT VALUE: %s" xml)
  (let ((status (coq-xml-val xml)))
    (pcase status
      ("fail"
       (let ((children (xml-node-children xml)))
	 (let ((error-msg (nth 1 children))) ; should be wrapped in string tags, bug 4849
	   (coq-server--clear-response-buffer)
	   (coq--display-response error-msg)))
       (coq-server--handle-error))
      ("good"
       (cond
	(coq-server--pending-edit-at-state-id ; response to Edit_at
	 ;; any spans contained between last end of focus and last tip
	 ;; check for backtrack past start of focus
	 (cond
	  ((coq-server--backtrack-before-focusp)
	   (message "BEFORE FOCUS")
	   ;; retract to before a re-opened proof
	   (assert proof-locked-secondary-span)
	   (coq-server--remove-secondary-locked-span t)
	   (setq coq-server--start-of-focus-state-id nil))
	  ((coq-server--value-new-focusp xml)
	   (message "NEW FOCUS")
	   ;; retract re-opens a proof
	   (coq-server--new-focus-backtrack xml))
	  ((coq-server--value-simple-backtrackp xml)
	   (message "SIMPLE BACKTRACK")
	   (coq-server--simple-backtrack))
	  (t
	   (error (format "Unexpected Edit_at response: %s" xml))))
	 ;; we're now in the requested Edit_at state id
	 (message "SETTING CURRENT STATE ID TO: %s" coq-server--pending-edit-at-state-id)
	 (setq coq-current-state-id coq-server--pending-edit-at-state-id)
	 (setq coq-server--pending-edit-at-state-id nil))
	((coq-server--value-end-focusp xml) 
	 ;; close of focus after Add
	 (message "CLOSE OF FOCUS")
	 (let ((qed-state-id (coq-server--end-focus-qed-state-id xml))
	       (new-tip-state-id (coq-server--end-focus-new-tip-state-id xml)))
	   (coq-set-span-state-id coq-server--current-span qed-state-id)
	   (setq coq-current-state-id new-tip-state-id)
	   (setq coq-server--start-of-focus-state-id nil)
	   (coq-server--merge-locked-spans)))
	((coq-server--value-init-state-idp xml) ; Init, get first state id
	 (message "INIT STATE ID")
	 (let ((state-id (coq-server--value-init-state-id xml)))
	   (setq coq-retract-buffer-state-id state-id)
	   (coq-server--update-state-id-and-process state-id)))
	((coq-server--value-new-state-idp xml) ; Add that updates state id
	 (message "ADD THAT UPDATES STATE ID")
	 (let ((state-id (coq-server--value-new-state-id xml)))
	   (coq-server--update-state-id-and-process state-id)))
	(t ; Add that doesn't update state id
	 (error "UNKNOWN GOOD VALUE RESPONSE")))))))

(defun coq-server--retract-on-error (error-span)
  ;; error on single Add, emulate CoqIDE
  ;; retract to last point
  (with-current-buffer proof-script-buffer
    (let* ((all-spans (overlays-in (point-min) (1- (span-start error-span))))
	   (state-id-spans (cl-remove-if-not 
			    (lambda (span) 
			      (span-property span 'state-id)) 
			    all-spans))
	   (sorted-state-id-spans (sort state-id-spans (lambda (sp1 sp2) (> (span-start sp1) (span-start sp2)))))
	   (state-id-span (car-safe sorted-state-id-spans))
	   (type-spans (cl-remove-if-not 
			(lambda (span) 
			  (span-property span 'type)) 
			all-spans))
	   (sorted-type-spans (sort type-spans (lambda (sp1 sp2) (> (span-start sp1) (span-start sp2)))))
	   (type-span (car-safe sorted-type-spans)))
      (proof-retract-until-point (span-end type-span)))))

;; if we've processed all the Add's, there will be at most 1 item in proof-action-list
;; TODO : is there a better way to discern this?
(defun coq-server--finished-adds ()
  (null (cdr proof-action-list)))

;; we see feedback messages twice, once for Goal, again for Status
;; see Bug 4850
;; process each one just once, because they have effects; use table to know if they've been seen
;; to prevent this table from taking too much space, we clear it just as each Add is sent
(defvar coq-server--feedback-errormsg-tbl (make-hash-table :test 'equal))

;; delay creating the XML so it will have the right state-id
;; the returned lambda captures the passed item, which is why 
;; this file needs lexical binding
;; side-effect of the thunk: clear feedback message table
(defun coq-server-make-add-command-thunk (cmd span)
  (lambda () 
    (clrhash coq-server--feedback-errormsg-tbl)
    (list (coq-xml-add-item cmd) span)))

(defun coq-server--handle-feedback (xml)
  (message "GOT FEEDBACK: %s" xml)
  (unless (gethash xml coq-server--feedback-errormsg-tbl)
    (puthash xml t coq-server--feedback-errormsg-tbl)
    (let ((feedback-type (coq-xml-at-path xml '(feedback (_) (feedback_content val)))))
      (message "feedback type: %s" feedback-type)
      (pcase feedback-type
	("errormsg"
	 (let* ((loc (coq-xml-at-path 
		      xml 
		      '(feedback (state_id) (feedback_content (loc)))))
		(error-start (string-to-number (coq-xml-attr-value loc 'start)))
		(error-stop (string-to-number (coq-xml-attr-value loc 'stop)))
		(msg-string (coq-xml-at-path 
			     xml 
			     '(feedback (state_id) (feedback_content (loc) (string)))))
		(error-msg (coq-xml-body1 msg-string))
		(error-state-id (coq-xml-at-path 
				 xml 
				 '(feedback (state_id val)))))
	   (pg-response-clear-displays)
	   (coq--display-response error-msg)
	   (let ((error-span (coq-server--find-span-with-state-id error-state-id)))
	     ;; coloring heuristic for errors
	     ;; if at end of locked span, do temp highlighting
	     ;; if error is in middle, indelibly color the span containing the error 
	     (if (coq-server--error-span-at-end-of-locked error-span)
		 (progn ; error in last sentence processed
		   (coq--highlight-error error-span error-start error-stop)
		   (coq-server--retract-on-error error-span))
	       ;; error in middle of processed region
	       (coq--mark-error error-span error-msg)))))
	;; TODO maybe use fancy colors for other feedbacks
	(t)))))

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
	(coq-server--handle-item xml)))))

;; process XML response from Coq
(defun coq-server-process-response (response span)
  (coq-server--append-response response)
  (with-current-buffer coq-server-response-buffer
    (coq-server--unescape-buffer))
  ;; maybe should pass this instead
  (setq coq-server--current-span span) 
  (let ((xml (coq-server--get-next-xml)))
    (while xml
      (pcase (coq-xml-tag xml)
	(`value (coq-server--handle-value xml))
	(`feedback (coq-server--handle-feedback xml))
	(`message (coq-server--handle-message xml))
	(t (message "unknown coqtop response %s" xml)))
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
