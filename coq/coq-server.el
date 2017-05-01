;;; -*- lexical-binding: t -*-

;; coq-server.el -- code related to server mode for Coq in Proof General

(require 'cl-lib)
(require 'xml)
(require 'thingatpt)

(require 'proof-queue)
(require 'proof-server)
(require 'proof-script)
(require 'pg-goals)

(require 'coq-tq)
(require 'coq-response)
(require 'coq-goals)
(require 'coq-xml)
(require 'coq-span)
(require 'coq-header-line)
(require 'coq-state-vars)
(require 'coq-company-compat)

;; add hook for debugging
(defvar coq-server-response-hook nil)

(defvar coq-server--pending-edit-at-state-id nil
  "State id for an Edit_at command sent, until we get a response.")

(defvar coq-server--pending-add-count 0)

(defvar coq-server--start-of-focus-state-id nil
  "When re-opening a proof, this is the state id of the current focus.
If we undo to point before the span with this state id, the focus 
is gone and we have to close the secondary locked span."
  )

(defvar coq-server--end-focus-retract-point nil
  "When user has edited secondary locked region, point to retract to
after closing focus")

(defvar coq-server--current-call nil
  "Call associated with last response")

(defvar coq-server--current-span nil
  "Span associated with last response")

(defvar coq-server--sticky-point nil
  "Cursor location after next Status")

(defvar coq-server-retraction-on-error nil
  "Was the last retraction due to an error")

(defvar coq-server-retraction-on-interrupt nil
  "Was the last retraction due to an interrupt")

(defvar coq-server--backtrack-on-failure nil
  "Was the last Edit_at backtrack due to a failure value")

(defvar coq-server-transaction-queue nil)

;; tag pair to know when complete response has been received
;; start tag may have attributes, so elide closing bracket
(defvar end-of-response-tags '("<value" . "</value>"))

;; sometimes we don't get a whole response, as when looping, so need
;;  to detect a usable partial response
;; need start, end tags to check they match
;; start tag may have attributes, so elide closing bracket
(defvar other-responses-tags '(("<feedback" . "</feedback>") ("<message" . "</message>")))

;; comments generate items with null element
;; helper for coq-server-count-pending-adds
(defun coq-server--count-addable (items) 
  (let ((ct 0))
    (mapc (lambda (it) (when (nth 1 it) (cl-incf ct))) items)
    ct))

;; hook function to count how many Adds are pending
(defun coq-server-incr-pending-adds (queue-items)
  (setq coq-server--pending-add-count
	(+ (coq-server--count-addable queue-items)
	   coq-server--pending-add-count)))

;; retract to particular state id, get Status, optionally get Goal
(defun coq-server--send-retraction (state-id &optional get-goal)
  (setq coq-server--pending-edit-at-state-id state-id)
  (proof-server-send-to-prover (coq-xml-edit-at state-id))
  (when get-goal
    (proof-server-send-to-prover (coq-xml-goal)))
  (proof-server-send-to-prover (coq-xml-status)))

(defun coq-server--clear-response-buffer ()
  (pg-response-clear-displays))

(defun coq-server-start-transaction-queue ()
  (setq coq-server-transaction-queue (coq-tq-create proof-server-process 'coq-server-process-oob
						end-of-response-tags other-responses-tags)))

;; stop all active workers
;; which we do when we get an interrupt and there's no pending response
(defun coq-server-stop-active-workers ()
  (maphash (lambda (worker-id _)
	     (proof-server-send-to-prover
	      (lambda ()
		(list 
		 (coq-xml-stop-worker worker-id)
		 nil))))
	   coq-worker-status-tbl))

;; clear response buffer when we Add an item from the Coq script
(add-hook 'proof-server-insert-hook 'coq-server--clear-response-buffer)

;; start transaction queue after coqtop process started
(add-hook 'proof-server-init-hook 'coq-server-start-transaction-queue)

;; run statements, rewind to state id just before statements sent
(defun coq-server-run-and-rewind (stmts handler)
  ;; when this function is called, may be other items on the transaction queue
  ;; save current state id when first statement sent
  (let (rewind-state-id)
    (dolist (stmt stmts)
      (proof-server-invisible-cmd-handle-result
       (lambda ()
	 (unless rewind-state-id
	   (setq rewind-state-id coq-current-state-id))
	 (list stmt nil))
       handler))
    (proof-server-send-to-prover
     (lambda ()
       (list (coq-xml-edit-at rewind-state-id) nil)))))

;; get Coq info from About call
(defun coq-server--value-about-p (xml)
  (coq-xml-at-path xml '(value (coq_info))))

(defun coq-server--get-coq-info (xml)
  (let* ((coq-info-items (coq-xml-body (coq-xml-at-path xml '(value (coq_info)))))
	 ;; items are Coq version, protocol version, compilation month/year,full compilation time
	 ;; version might be "trunk", so not so useful
	 (coq-protocol-version-string (nth 1 coq-info-items)))
    (setq coq-xml-protocol-date (coq-xml-body1 coq-protocol-version-string))
    (coq-xml-set-special-tokens)))

;; update global state in response to status
(defun coq-server--handle-status (_xml)
  (when coq-server--sticky-point
    (with-current-buffer proof-script-buffer
      (goto-char coq-server--sticky-point))
    (setq coq-server--sticky-point nil)))

;; unit response
(defvar coq-server--value-unit-footprint '(value (unit)))

(defun coq-server--value-unit-p (xml)
  (equal (coq-xml-footprint xml)
	 coq-server--value-unit-footprint))

;; no current goal
(defvar coq-server--value-empty-goals-footprint
  '(value (option)))

(defun coq-server--value-empty-goals-p (xml)
  (equal (coq-xml-footprint xml) 
	 coq-server--value-empty-goals-footprint))

(defun coq-server--handle-empty-goals ()
  (setq proof-prover-proof-completed 0)
  (setq proof-shell-last-goals-output "")
  (coq-goals-clear-goals-buffer))

;; use path instead of footprint, because inner bits may vary
(defun coq-server--value-goals-p (xml)
  (coq-xml-at-path xml '(value (option (goals)))))

;; was the last goal response not the empty goal?
(defvar coq-server--last-goal-nonempty)

(defun coq-server--handle-goals (xml)
  (setq proof-prover-proof-completed nil)
  (let* ((show-subgoals (not coq-hide-additional-subgoals))
	 (all-goals (coq-xml-body (coq-xml-at-path xml '(value (option (goals))))))
	 (all-current-goals (coq-xml-body (nth 0 all-goals)))
	 (current-goals (if show-subgoals
			    all-current-goals
			  (list (car all-current-goals))))
	 ;; background goals have a different, extra structure than the other ones
	 (bg-goals-pairs (coq-xml-body (nth 1 all-goals)))
	 (before-bg-goals nil)
	 (after-bg-goals nil)
	 (shelved-goals (coq-xml-body (nth 2 all-goals)))
	 (abandoned-goals (coq-xml-body (nth 3 all-goals)))
	 goal-text)
      (when (and abandoned-goals show-subgoals)
	(setq goal-text (cons (coq-goals-make-goals-string abandoned-goals nil t 'Abandoned) goal-text)))
      (when (and shelved-goals show-subgoals)
	(setq goal-text (cons (coq-goals-make-goals-string shelved-goals nil t 'Shelved) goal-text)))
      (when (and bg-goals-pairs show-subgoals)
	(dolist (pair bg-goals-pairs)
	  (let* ((before-focus-pair-list (nth 0 (coq-xml-body pair)))
		 (after-focus-pair-list (nth 1 (coq-xml-body pair)))
		 (before-pair-goals (reverse (coq-xml-body before-focus-pair-list)))
		 (after-pair-goals (coq-xml-body after-focus-pair-list)))
	    (setq before-bg-goals (append before-bg-goals before-pair-goals))
	    (setq after-bg-goals (append after-bg-goals after-pair-goals))))
	;; cons after goals, then before goals, so see before, then after in output
	(when after-bg-goals 
	  (setq goal-text (cons (coq-goals-make-goals-string after-bg-goals nil t "Unfocused (after focus)") goal-text)))
	(when before-bg-goals 
	  (setq goal-text (cons (coq-goals-make-goals-string before-bg-goals nil t "Unfocused (before focus)") goal-text))))
      (when current-goals
	(setq goal-text (cons (coq-goals-make-goals-string current-goals (length all-current-goals)) goal-text)))
      (if goal-text
	  (let ((formatted-goals (mapconcat 'identity goal-text "\n\n")))
	    (setq coq-server--last-goal-nonempty t)
	    (setq proof-shell-last-goals-output formatted-goals)
	    (coq-goals-show-goals formatted-goals))
	;; else, clear goals display
	(coq-goals-clear-goals-buffer)
	;; mimic the coqtop REPL, though it would be better to come via XML
	(when coq-server--last-goal-nonempty
	  (setq coq-server--last-goal-nonempty nil)
	  (coq-display-response "No more subgoals.")))))

(defun coq-server--value-status-p (xml)
  (coq-xml-at-path xml '(value (status))))

;; make pending Edit_at state id current
(defun coq-server--make-edit-at-state-id-current ()
  (setq coq-current-state-id coq-server--pending-edit-at-state-id)
  (setq coq-server--pending-edit-at-state-id nil))

(defvar coq-server--value-simple-backtrack-footprint 
  '(value (union (unit))))

(defun coq-server--value-simple-backtrack-p (xml)
  (and coq-server--pending-edit-at-state-id 
       (equal (coq-xml-footprint xml)
	      coq-server--value-simple-backtrack-footprint)))

;; Edit_at, get new focus
(defvar coq-server--value-new-focus-footprint 
  '(value (union (pair (state_id) (pair (state_id) (state_id))))))

(defun coq-server--value-new-focus-p (xml)
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

(defun coq-server--value-init-state-id-p (xml)
  (equal (coq-xml-footprint xml) 
	 coq-server--value-init-state-id-footprint))

(defun coq-server--value-init-state-id (xml)
  (coq-xml-at-path 
   xml
   '(value (state_id val))))

(defun coq-server--set-init-state-id (xml)
  (let ((state-id (coq-server--value-init-state-id xml)))
    (setq coq-retract-buffer-state-id state-id)
    (coq-server--update-state-id state-id)))

;; value when updating state id from an Add
(defvar coq-server--value-new-state-id-footprint
  '(value (pair (state_id) (pair (union (unit)) (string)))))

(defun coq-server--value-new-state-id-p (xml)
  (equal (coq-xml-footprint xml) 
	 coq-server--value-new-state-id-footprint))

(defun coq-server--set-new-state-id (xml)
  (let ((state-id (coq-xml-at-path 
		   xml
		   '(value (pair (state_id val))))))
    (coq-server--update-state-id-and-process state-id)))

;; Add'ing past end of focus
(defvar coq-server--value-end-focus-footprint 
  '(value (pair (state_id) (pair (union (state_id)) (string)))))

(defun coq-server--value-end-focus-p (xml) 
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

(defun coq-server--set-span-state-id (span val)
  (span-set-property span 'state-id val))

(defun coq-server--register-state-id (span state-id)
  (coq-server--set-span-state-id span state-id)
  (puthash state-id span coq-span-state-id-tbl))

(defun coq-server--end-focus (xml)
  (let ((qed-state-id (coq-server--end-focus-qed-state-id xml))
	(new-tip-state-id (coq-server--end-focus-new-tip-state-id xml)))
    (coq-server--register-state-id coq-server--current-span qed-state-id)
    (coq-server--process-queued-feedbacks qed-state-id)
    (setq coq-server--start-of-focus-state-id nil)
    ;; prevents current span from getting new tip state id
    (setq coq-server--current-span nil) 
    (when proof-locked-secondary-span
      (coq-server--merge-locked-spans))
    (coq-server--update-state-id-and-process new-tip-state-id)
    ;; if user has edited within secondary locked region, retract to edit point
    (when coq-server--end-focus-retract-point
      (with-current-buffer proof-script-buffer
	(goto-char coq-server--end-focus-retract-point)
	(setq coq-server--end-focus-retract-point nil)
	(proof-retract-until-point)))))

(defvar coq-server--retract-error nil)

;; show error after a retract
;; span will be detached after the retract, so save its endpoints
(defun coq-server--queue-retract-error (span start end msg)
  (setq coq-server--retract-error
	(list msg
	      (cons (span-start span) (span-end span))
	      (cons start end))))

(defun coq-server--show-retract-error ()
  (when coq-server--retract-error
    (let* ((msg (nth 0 coq-server--retract-error))
	   (endpoints (nth 1 coq-server--retract-error))
	   (start (car endpoints))
	   (end (cdr endpoints))
	   (locs (nth 2 coq-server--retract-error))
	   (error-start (car locs))
	   (error-end (cdr locs)))
      (setq coq-server--retract-error nil)
      (coq-mark-error start end error-start error-end msg t))))

(defun coq-server--simple-backtrack ()
  ;; delete spans marked for deletion
  (with-current-buffer proof-script-buffer
    (let* ((retract-span (coq-server--get-span-with-state-id coq-server--pending-edit-at-state-id))
	   (start (or (and retract-span (1+ (span-end retract-span)))
		      (point-min)))
	   (sent-end (or (and retract-span (span-end retract-span))
			 (point-min)))
	   (spans-after-retract (spans-in start (point-max))))
      (if coq-server--backtrack-on-failure
	  (progn 
	    (setq coq-server--backtrack-on-failure nil)
	    ;; if there is a secondary locked region, the failure must have been in the primary locked region
	    ;; control jumps there, so let user know this is unexpected
	    (when proof-locked-secondary-span
	      (message "Error when processing proof to close focus"))
	    ;; if we backtracked on a failure, see if the next span with a state id
	    ;; has a pg-error span on top; if so, unmark it for deletion
	    (let* ((relevant-spans (cl-remove-if-not
				    (lambda (sp) (span-property sp 'marked-for-deletion))
				    spans-after-retract))
		   (sorted-spans (sort relevant-spans
				       (lambda (sp1 sp2) (< (span-start sp1) (span-start sp2)))))
		   error-span
		   state-id-span)
	      ;; see if first pg-error span overlaps first span with a state id
	      (while (and sorted-spans (or (null error-span) (null state-id-span)))
		(let ((span (car sorted-spans)))
		  (when (span-property span 'state-id)
		    (setq state-id-span span))
		  (when (eq (span-property span 'type) 'pg-error)
		    (setq error-span span)))
		(setq sorted-spans (cdr sorted-spans)))
	      (when (and state-id-span error-span
			 (>= (span-start error-span) (span-start state-id-span))
			 (<= (span-end error-span) (span-end state-id-span)))
		(span-unmark-delete error-span))))
	;; not on failure, delete pg-error spans below
	(let* ((error-spans (cl-remove-if-not
			     (lambda (sp) (eq (span-property sp 'type)
					      'pg-error))
			    spans-after-retract)))
	  (mapc 'span-delete error-spans)))
      ;; now remove spans marked for deletion
      (mapc (lambda (span)
	      (when (or (and (span-property span 'marked-for-deletion)
			     (not (span-property span 'self-removing))))
		(span-delete span)))
	    (spans-all))
      (proof-set-locked-end sent-end)
      (proof-set-sent-end sent-end)))
  (coq-server--show-retract-error)
  (coq-error-set-update)
  (coq-server--make-edit-at-state-id-current))

(defun coq-server--new-focus-backtrack (xml)
  ;; new focus produces secondary locked span, which extends from
  ;; end of new focus to last tip
  ;; primary locked span is from start of script to the edit at state id
  ;; want a secondary locked span just past focus end to old tip
  (let* ((state-ids (coq-server--new-focus-state-ids xml))
	 (focus-start-state-id (nth 0 state-ids))
	 (focus-end-state-id (nth 1 state-ids))
	 (last-tip-state-id (nth 2 state-ids)))
    ;; if focus end and last tip are the same, treat as simple backtrack
    (if (equal focus-end-state-id last-tip-state-id)
	(coq-server--simple-backtrack)
      ;; multiple else's
      (setq coq-server--start-of-focus-state-id focus-start-state-id)
      (coq-server--create-secondary-locked-span focus-start-state-id focus-end-state-id last-tip-state-id)
      (coq-server--show-retract-error)
      (coq-error-set-update)
      (coq-server--make-edit-at-state-id-current))))

(defun coq-server--secondary-locked-handler (span after-p start end &optional len)
  "Handler for edits within secondary locked region"
  (when after-p
    (let ((pt (if (= 0 len)
		  end
		start)))
      (setq coq-server--end-focus-retract-point pt)
      (proof-assert-until-point (span-end span)))))

(defun coq-server--create-secondary-locked-span (focus-start-state-id focus-end-state-id last-tip-state-id)
  (with-current-buffer proof-script-buffer
    (let* ((focus-start-span (coq-server--get-span-with-state-id focus-start-state-id))
	   (focus-end-span (coq-server--get-span-with-state-id focus-end-state-id))
	   (last-tip-span (coq-server--get-span-with-state-id last-tip-state-id))
	   (all-spans (spans-in (span-start focus-start-span) (span-end last-tip-span)))
	   (marked-spans (cl-remove-if-not 
			  (lambda (span) (span-property span 'marked-for-deletion))
			  all-spans))
	   (sorted-marked-spans 
	    (sort marked-spans (lambda (sp1 sp2) (< (span-start sp1) (span-start sp2)))))
	   (focus-spans (spans-in (span-start focus-start-span) (span-end focus-end-span)))
	   (error-spans (cl-remove-if-not (lambda (span) (eq (span-property span 'type) 'pg-error)) focus-spans))
	   found-focus-end
	   secondary-span-start
	   secondary-span-end)
      ;; delete any error spans within focus
      (mapc 'span-delete error-spans)
      ;; tentatively set end of secondary span
      (setq secondary-span-end (span-end last-tip-span))
      ;; delete marked spans within focus, because they're unprocessed now
      ;; but leave spans beneath the focus, because we'll skip past them 
      ;;  when merging primary, secondary locked regions
      (dolist (span sorted-marked-spans)
	(if (and found-focus-end
		 ;; even though we've found the focus end, there
		 ;; may be colored spans atop the focus end, which should 
		 ;; be deleted, so check that these spans are past the end
		 (>= (span-end span) secondary-span-start))
	    ;; don't delete the span 
	    (span-unmark-delete span)
	  ;; look for focus end
	  (let ((span-state-id (span-property span 'state-id)))
	    (when (and span-state-id (equal span-state-id focus-end-state-id))
	      (let ((curr-span-end (span-end span)))
		;; the secondary span starts just past the focus end
		(unless secondary-span-start 
		  (setq secondary-span-start (1+ curr-span-end))))
	      (setq found-focus-end t))
	    (span-delete span))))
      ;; skip past whitespace for secondary span
      (save-excursion
	(goto-char secondary-span-start)
	(skip-chars-forward " \t\n")
	(beginning-of-thing 'line)
	(setq secondary-span-start (point)))
      (let* ((span (span-make secondary-span-start secondary-span-end)))
	(span-set-property span 'face 'proof-secondary-locked-face)
	(span-set-priority span (gethash proof-secondary-locked-face coq-face-rank-tbl))
	(span-set-property span 'modification-hooks (list 'coq-server--secondary-locked-handler))
	(setq proof-locked-secondary-span span)))))

(defun coq-server--remove-secondary-locked-span (&optional delete-spans)
  (let ((start (span-start proof-locked-secondary-span))
	(end (span-end proof-locked-secondary-span)))
    ;; remove read-only property
    (with-current-buffer proof-script-buffer
      (span-delete proof-locked-secondary-span)
      (setq proof-locked-secondary-span nil)
      (setq inhibit-read-only t) ; "special trick"
      (remove-list-of-text-properties start end (list 'read-only))
      (setq inhibit-read-only nil)
      ;; delete unless merging primary, secondary locked regions 
      ;; spans following primary locked region are valid
      (when delete-spans
	(let* ((candidate-spans (spans-in start end))
	       (relevant-spans 
		(cl-remove-if-not 
		 (lambda (span) (or (span-property span 'type) (span-property span 'idiom)))
		 candidate-spans)))
	  (mapc 'span-delete relevant-spans))))))

(defun coq-server--merge-locked-spans ()
  (with-current-buffer proof-script-buffer
    (let ((new-end (span-end proof-locked-secondary-span)))
      (coq-server--remove-secondary-locked-span)
      (proof-merge-locked new-end))))

;; did we backtrack to a point before the current focus
(defun coq-server--backtrack-before-focus-p (xml)
  (and (coq-server--value-simple-backtrack-p xml) ; response otherwise looks like simple backtrack
       coq-server--start-of-focus-state-id 
       (or (equal coq-server--pending-edit-at-state-id coq-retract-buffer-state-id)
	   (coq-server--state-id-precedes 
	    coq-server--pending-edit-at-state-id 
	    coq-server--start-of-focus-state-id))))

(defun coq-server--before-focus-backtrack ()
  ;; retract to before a re-opened proof
  (cl-assert proof-locked-secondary-span)
  (coq-server--remove-secondary-locked-span t)
  (setq coq-server--start-of-focus-state-id nil)
  (coq-error-set-update)
  (coq-server--make-edit-at-state-id-current))

(defun coq-server--update-state-id (state-id)
  (setq coq-current-state-id state-id)
  (when coq-server--current-span
    (coq-server--register-state-id coq-server--current-span state-id)
    (coq-server--process-queued-feedbacks state-id)))

(defun coq-server--update-state-id-and-process (state-id)
  (coq-server--update-state-id state-id)
  (when (> coq-server--pending-add-count 0)
    (setq coq-server--pending-add-count (1- coq-server--pending-add-count)))
  ;; gotten response from all Adds, ask for goals/status
  (when (= coq-server--pending-add-count 0)
    ;; if we've gotten responses from all Add's, ask for goals/status
    (proof-server-send-to-prover (coq-xml-goal))
    (proof-server-send-to-prover (coq-xml-status))))

;; no backtrack on Query call (Coq bug #5041)
(defun coq-server--was-query-call ()
  ;; always unescape to space, default unescaping might be different
  (let ((xml (coq-xml-string-to-xml
	      (coq-xml-unescape-string coq-server--current-call " "))))
    (when xml
      (let ((call-val (coq-xml-at-path xml '(call val))))
	(equal call-val "Query")))))

(defun coq-server--was-check-document-call ()
  (let ((xml (coq-xml-string-to-xml
	      (coq-xml-unescape-string coq-server--current-call " "))))
    (when xml
      (let ((call-val (coq-xml-at-path xml '(call val)))
	    (bool-val (coq-xml-at-path xml '(call (bool val)))))
	(and (equal call-val "Status")
	     (equal bool-val "true"))))))

(defun coq-server--valid-state-id (state-id)
  (not (equal state-id "0")))

(defun coq-server--backtrack-to-valid-state (valid-state-id)
  (unless (coq-server--valid-state-id valid-state-id)
    ;; best guess at a valid state
    (setq valid-state-id coq-current-state-id))
  (with-current-buffer proof-script-buffer
    (if (equal valid-state-id coq-retract-buffer-state-id)
	(goto-char (point-min))
      (let ((valid-span (coq-server--get-span-with-state-id valid-state-id)))
	(if (and valid-span (span-buffer valid-span))
	    (goto-char (span-end valid-span))
	  ;; can't find span to retract to, punt
	  (goto-char (point-min)))))
    (proof-goto-point)))

(defun coq-server--handle-failure-value (xml)
  ;; flush queue of actions
  (proof-server-clear-state)
  ;; flush feedback queues
  (clrhash coq-feedbacks-tbl)
  ;; remove pending calls to Coq, except for the one that
  ;; generated this failure, which gets popped when control
  ;; returns to tq-process-buffer
  (coq-tq-flush-but-1 coq-server-transaction-queue)
  ;; no more pending Adds 
  (setq coq-server--pending-add-count 0)
  (unless (coq-server--was-query-call)
    ;; in case it was an Edit_at that failed
    (setq coq-server--pending-edit-at-state-id nil)
    ;; don't move point if it was a document check error
    (unless (coq-server--was-check-document-call)
      (with-current-buffer proof-script-buffer
	(setq coq-server--sticky-point (point))))
    ;; don't clear pending edit-at state id here
    ;; because we may get failures from Status/Goals before the edit-at value
    ;; we usually see the failure twice, once for Goal, again for Status
    (let* ((last-valid-state-id (coq-xml-at-path xml '(value (state_id val))))
	   (loc-start (let ((pos (coq-xml-at-path xml '(value loc_s))))
			(and pos (string-to-number pos))))
	   (loc-end (let ((pos (coq-xml-at-path xml '(value loc_e))))
		      (and pos (string-to-number pos))))
	   (error-msg
	    (pcase (coq-xml-protocol-version)
	      ((pred coq-xml-protocol-8.5-p)
	       ;; can't use coq-xml-at-path, message not in tags (see bug 4849)
	       (cadr (cdr (cdr xml))))
	      ((pred coq-xml-protocol-8.6-p)
	       (let ((richpp-text (coq-xml-at-path xml '(value (_) (richpp (_))))))
		 (and richpp-text
		      (coq-xml-flatten-pp (coq-xml-body richpp-text)))))
	      (_ (coq-xml-bad-protocol))))
	   (error-span (gethash last-valid-state-id coq-span-add-call-state-id-tbl)))
      ;; queue this error for retract finish
      ;; if we mark error now, it will just disappear 
      (when error-span
	(coq-server--queue-retract-error
	 error-span
	 (or loc-start 0)
	 (or loc-end (- (span-end error-span) (span-start error-span)))
	 error-msg))
      (coq-server--clear-response-buffer)
      (coq-display-response error-msg)
      (unless (gethash xml coq-error-fail-tbl)
	(puthash xml t coq-error-fail-tbl)
	(setq coq-server--backtrack-on-failure t)
	(setq coq-server-retraction-on-error t)
	(coq-server--backtrack-to-valid-state last-valid-state-id)))))

(defun coq-server--handle-good-value (xml)
  (cond
   ((coq-server--value-new-state-id-p xml) 
    ;; Add that updates state id
    (coq-server--set-new-state-id xml))

   ((coq-server--value-status-p xml)
    (coq-server--handle-status xml))

   ((coq-server--value-goals-p xml)
    ;; Response to Goals, some current goals
    (coq-server--handle-goals xml))

   ((coq-server--value-empty-goals-p xml)
    ;; Response to Goals, with no current goals
    (coq-server--handle-empty-goals))

   ((coq-server--value-unit-p xml)
    ;; unit response, nothing to do
    nil)
   
   ((coq-server--value-simple-backtrack-p xml)
    ;; simple backtrack
    (coq-server--simple-backtrack))

   ((coq-server--backtrack-before-focus-p xml)
    ;; retract before current focus
    (coq-server--before-focus-backtrack))

   ((coq-server--value-new-focus-p xml)
    ;; retract re-opens a proof, creates focus
    (coq-server--new-focus-backtrack xml))

   ((coq-server--value-end-focus-p xml) 
    ;; close of focus after Add
    (coq-server--end-focus xml))

   ((coq-server--value-init-state-id-p xml) 
    ;; Init, get first state id
    (coq-server--set-init-state-id xml))

   ((coq-server--value-about-p xml)
    ;; About, get Coq info
    (coq-server--get-coq-info xml))
   
   ;; some good values are unprocessed, for example, responses to Query 
   (t (error (format "Unknown good value: %s" xml)))))

;; we distinguish value responses by their syntactic structure
;; and a little bit by some global state
;; can we do better?
(defun coq-server--handle-value (xml)
  (let ((status (coq-xml-val xml)))
    (pcase status
      ("good"
       (coq-server--handle-good-value xml))
      ("fail"
       (coq-server--handle-failure-value xml)))))

;; delay creating the XML so it will have the right state-id
;; the returned lambda captures the passed item, which is why 
;; this file needs lexical binding
;; side-effect of the thunk: clear feedback message table
(defun coq-server-make-add-command-thunk (cmd span)
  (lambda () 
    (clrhash coq-error-fail-tbl)
    (list (coq-xml-add-item cmd) span)))

;; error coloring heuristic 
(defun coq-server--error-span-at-end-of-locked (error-span)
  ;; proof-locked-span may be detached, so lookup needed span
  (let* ((locked-span (coq-server--get-span-with-predicate
		       (lambda (span)
			 (equal (span-property span 'face) 'proof-locked-face))
		       ;; locked region always begins at point-min
		       (overlays-at (point-min))))
	 (locked-end (or (and locked-span (span-end locked-span)) 0))
	 (error-end (span-end error-span)))
    (>= error-end locked-end)))

(defun coq-server--display-error (error-state-id error-edit-id error-msg error-start error-stop)
  (if (and (or (null error-state-id)
	       (not (coq-server--valid-state-id error-state-id)))
	   (null error-edit-id))
      ;; no context for this error      
      (progn
	(coq-server--clear-response-buffer)
	(coq-display-response error-msg))
    (let ((error-span (or (coq-server--get-span-with-state-id error-state-id)
			  (coq-server--get-span-with-edit-id error-edit-id)
			  ;; no span associated with state id or edit id
			  ;; assume current span
			  coq-server--current-span)))
      ;; decide where to show error 
      ;; on subsequent retraction, keep error in response buffer
      (setq coq-server-retraction-on-error t) 
      (if (coq-server--error-span-at-end-of-locked error-span)
	  (progn
	    (coq-server--clear-response-buffer)
	    (coq-display-response error-msg)
	    (setq coq-server--sticky-point (coq--highlight-error error-span error-start error-stop)))
	;; error in middle of processed region
	;; indelibly color the error 
	(coq-mark-error (span-start error-span) (span-end error-span)
			error-start error-stop error-msg)))))

;; this is for 8.5
(defun coq-server--handle-errormsg (xml)
  ;; memoize this errormsg response
  (puthash xml t coq-error-fail-tbl)
  (let* ((loc (coq-xml-at-path 
	       xml 
	       '(feedback (_) (feedback_content (loc)))))
	 (error-start (string-to-number (coq-xml-attr-value loc 'start)))
	 (error-stop (string-to-number (coq-xml-attr-value loc 'stop)))
	 (msg-string (coq-xml-at-path 
		      xml 
		      '(feedback (state_id) (feedback_content (loc) (string)))))
	 (error-msg (coq-xml-body1 msg-string))
	 (error-state-id (coq-xml-at-path 
			  xml 
			  '(feedback (state_id val))))
	 (error-edit-id (coq-xml-at-path 
			 xml 
			 '(feedback (edit_id val)))))
    ;; may get either state id or edit id
    ;; can get error message not associated with script text
    (coq-server--display-error error-state-id error-edit-id error-msg error-start error-stop)))

;; this is for 8.6
(defun coq-server--handle-error (xml)
  ;; memoize this response
  (puthash xml t coq-error-fail-tbl)
  ;; TODO what happens when there's no location?
  ;; can get a state id or edit id, so use _ in path
  (let* ((loc (coq-xml-at-path
	       xml
	       '(feedback (_) (feedback_content (message (message_level) (option (loc)))))))
	 (error-start (string-to-number (coq-xml-attr-value loc 'start)))
	 (error-stop (string-to-number (coq-xml-attr-value loc 'stop)))
	 (msg-string (coq-xml-at-path 
		      xml
		      '(feedback (_) (feedback_content (message (message_level) (option (loc)) (richpp (_)))))))
	 (error-msg (coq-xml-flatten-pp (coq-xml-body msg-string)))
	 (error-state-id (coq-xml-at-path 
			  xml 
			  '(feedback (state_id val))))
	 (error-edit-id (coq-xml-at-path 
			 xml 
			 '(feedback (edit_id val)))))
    ;; may get either state id or edit id
    ;; may get error message not associated with script text
    (coq-server--display-error error-state-id error-edit-id error-msg error-start error-stop)))

;; maintain table of active workers
(defun coq-server--handle-worker-status (xml)
  (let* ((status-pair (coq-xml-at-path xml '(feedback (state_id) (feedback_content (pair)))))
	 (worker-id (coq-xml-body1 (coq-xml-at-path status-pair '(pair (string)))))
	 (status (coq-xml-body1 (coq-xml-at-path status-pair '(pair (string) (string))))))
    (if (member status '("Idle" "Dead"))
	(remhash worker-id coq-worker-status-tbl)
      (puthash worker-id status coq-worker-status-tbl))))

(defun coq-server--handle-filedependency (xml)
  ;; don't bother parsing unless we're printing the debug msg
  (when proof-general-debug-messages
    (let* ((content (coq-xml-at-path xml '(feedback (_) (feedback_content))))
	   (from-option (coq-xml-at-path content '(feedback_content (option val))))
	   (from (if (equal from-option "some")
		     (coq-xml-body1 (coq-xml-at-path content '(feedback_content (option (string)))))
		   "<None>"))
	   (file-string (coq-xml-at-path content '(feedback_content (option) (string))))
	   (file (coq-xml-body1 file-string)))
      (proof-debug-message "File dependency, from: %s dep: %s" from file))))

(defun coq-server--handle-fileloaded (xml)
  ;; don't bother parsing unless we're printing the debug msg
  (when proof-general-debug-messages
    (let* ((contents (coq-xml-body (coq-xml-at-path xml '(feedback (_) (feedback_content)))))
	   (namespace-string (nth 0 contents))
	   (namespace (coq-xml-body1 namespace-string))
	   (file-string (nth 1 contents))
	   (file (coq-xml-body1 file-string)))
      (proof-debug-message "File loaded, namespace: %s file: %s" namespace file))))

;; queue feedback if there's no span with its state id
;;  for later processing
(defun coq-server--queue-feedback (state-id xml)
  (let ((queue (gethash state-id coq-feedbacks-tbl)))
    (puthash state-id (cons xml queue) coq-feedbacks-tbl)))

;; process queued feedbacks in order of arrival
;; earliest item is at end of list
(defun coq-server--process-queued-feedbacks (state-id)
  ;; some feedbacks re workers have the initial state id
  ;;  which has no associated span
  (unless (equal state-id coq-retract-buffer-state-id)
    (let ((queue (gethash state-id coq-feedbacks-tbl)))
      (mapc 'coq-server--handle-feedback (reverse queue)))
    ;; flush queue for this state id
    (remhash state-id coq-feedbacks-tbl)))

(defun coq-server--handle-feedback (xml)
  (let* ((state-id (coq-xml-at-path xml '(feedback (state_id val))))
	 (span-with-state-id (and state-id ; might have edit_id
				  (coq-server--get-span-with-state-id state-id))))
    (if (and state-id
	     ;; don't queue if dummy state id
	     (not (equal state-id "0")) 
	     (or (null span-with-state-id)
		 ;; may have since-deleted span in table
		 (null (overlay-buffer span-with-state-id))))
	(coq-server--queue-feedback state-id xml)
      (pcase (coq-xml-at-path xml '(feedback (_) (feedback_content val)))
	("filedependency"
	 (coq-server--handle-filedependency xml))
	("fileloaded"
	 (coq-server--handle-fileloaded xml))
	("processingin"
	 (coq-span-color-span-processingin xml))
	("processed"
	 (coq-span-color-span-processed xml))
	("incomplete"
	 (coq-span-color-span-incomplete xml))
	("complete"
	 (coq-span-color-span-complete xml))
	("workerstatus"
	 (coq-server--handle-worker-status xml))
	("errormsg" ; 8.5-only
	 (unless (gethash xml coq-error-fail-tbl)
	   (coq-server--handle-errormsg xml)))
	("message" ; 8.6
	 (let ((msg-level 
		(coq-xml-at-path 
		 xml 
		 '(feedback (_) (feedback_content (message (message_level val)))))))
	   (cond 
	    ((member msg-level '("error" "warning"))
	     (unless (gethash xml coq-error-fail-tbl)
	       (coq-server--handle-error xml)))
	    ((member msg-level '("notice" "info" "debug"))
	      (let ((richpp (coq-xml-at-path
			     xml
			     '(feedback (_) (feedback_content
					     (message (message_level) (option) (richpp (_))))))))
		(coq-display-response (coq-xml-flatten-pp (coq-xml-body richpp))))))))
	(t)))))

;; syntax of messages differs in 8.5 and 8.6
(defun coq-server--handle-message (xml)
  (let* ((message
	  (pcase (coq-xml-protocol-version)
	    ((pred coq-xml-protocol-8.5-p)
	     (coq-xml-body1 (coq-xml-at-path xml '(message (message_level) (string)))))
	    ((pred coq-xml-protocol-8.6-p)
	     ;; The _ below is a wildcard in our search path, but the tag is actually _
	     ;; something of a delicious irony
	     (coq-xml-flatten-pp
	      (coq-xml-body
	       (coq-xml-at-path xml '(message (message_level) (option) (richpp (_)))))))
	    (_ (coq-xml-bad-protocol)))))
    (coq-display-response message)))

(defun coq-server--xml-parse (response call span)
  ;; claimed invariant: response is well-formed XML
  (when response
    (insert response)
    (coq-xml-unescape-buffer)
    (setq coq-server--current-call call) 
    (setq coq-server--current-span span) 
    (let ((xml (coq-xml-get-next-xml)))
      (run-hook-with-args coq-server-response-hook xml)
      (pcase (coq-xml-tag xml)
	;; feedbacks are most common, so put first here
	(`feedback (coq-server--handle-feedback xml))
	(`value (coq-server--handle-value xml))
	(`message (coq-server--handle-message xml))
	(t (proof-debug-message "Unknown coqtop response %s" xml))))
    (when (> (buffer-size) 0)
      ;; since current reponse invalid, don't send anything more
      (coq-tq-flush coq-server-transaction-queue)
      (message "*** Ill-formed XML:\n%s\n*** End of ill-formed XML" (buffer-string))
      (erase-buffer)
      (let ((warning "Warning: received ill-formed XML from Coq.")
	    (advice "Goto an earlier good point in the script to resync.")
	    (cmd "To help diagnose the issue, enable logging with \"M-x proof-server-enable-logging\"."))
	(message-box "%s\n\n%s\n\n%s" warning advice cmd)))))

;; process XML response from Coq
(defun coq-server-process-response (response call span)
  (with-current-buffer coq-xml-response-buffer
    (coq-server--xml-parse response call span)))

;; process OOB response from Coq
(defun coq-server-process-oob (_closure oob call span)
  (with-current-buffer coq-xml-oob-buffer
    (coq-server--xml-parse oob call span)))

(defun coq-server-handle-tq-response (special-processor response call span)
  ;; if there's a special processor, use that
  (if special-processor
      (funcall special-processor response call span)
    (coq-server-process-response response call span))
  ;; advance script queue
  (proof-server-manage-output response))

;; send data to Coq by sending to process
;; called by proof-server-send-to-prover
;; do not call directly
(defun coq-server-send-to-prover (s special-handler)
  (if coq-server-transaction-queue
    (coq-tq-enqueue coq-server-transaction-queue s
		    ;; "closure" argument, passed to handler below
		    ;; can be used for unusual processing on response
		    ;; for example, to insert intros into script
		    ;; always nil or a function symbol
		    special-handler
		    ;; default handler gets special-handler and coqtop response
		    #'coq-server-handle-tq-response)
    ;; discard item if transaction queue not initialized
    ;; can happen, for example, if user does Insert Intros before Coq started
    (message "Coq not started")))

(provide 'coq-server)
