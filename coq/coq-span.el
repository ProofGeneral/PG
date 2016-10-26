;; coq-span.el -- coloring for feedbacks, errors, and other span utilities

(require 'proof-script)
(require 'coq-state-vars)
(require 'coq-xml)
(require 'coq-header-line)

(defun coq-server--state-id-precedes (state-id-1 state-id-2)
  "Does STATE-ID-1 occur in a span before that for STATE-ID-2?"
  (let ((span1 (coq-server--get-span-with-state-id state-id-1))
	(span2 (coq-server--get-span-with-state-id state-id-2)))
    (< (span-start span1) (span-start span2))))

(defun coq-server--get-span-with-predicate (pred &optional span-list)
  (with-current-buffer proof-script-buffer
    (let* ((all-spans (or span-list (spans-all))))
      (cl-find-if pred all-spans))))

;; we could use the predicate mechanism, but this happens a lot
;; so use hash table
(defun coq-server--get-span-with-state-id (state-id)
  (gethash state-id coq-span-state-id-tbl))

(defun coq-server--get-span-with-edit-id (edit-id)
  (gethash edit-id coq-span-edit-id-tbl))

(defun coq-span-color-span-on-feedback (xml tbl prop face)
    (let* ((state-id (coq-xml-at-path xml '(feedback (state_id val))))
	   (span-with-state-id (coq-server--get-span-with-state-id state-id)))
      ;; can see feedbacks with state id not yet associated with a span
      ;; also can find a span with a state id that's been deleted from script buffer,
      ;;  but not yet garbage-collected from table
      (when (and span-with-state-id
		 (eq (span-buffer span-with-state-id) proof-script-buffer))
	(with-current-buffer proof-script-buffer
	  (save-excursion
	    (goto-char (span-start span-with-state-id))
	    (skip-chars-forward " \t\n")
	    ;; if there's a "sent" span here, remove it, because now we have
	    ;; relevant feedback
	    (let* ((spans-here (overlays-at (point)))
		   (sent-spans (cl-remove-if-not (lambda (sp) (span-property sp 'sent))
						 spans-here)))
	      (mapc 'span-delete sent-spans))
	    ;; if there's an existing colored span at point, re-use it,
	    ;;  because want most recent coloring
	    (let ((span-processing (or (gethash state-id tbl)
				       (span-make (point) (span-end span-with-state-id))))
		  (rank (gethash face face-rank-tbl)))
	      (span-set-property span-processing 'type 'pg-special-coloring)
	      (span-set-property span-processing prop 't)
	      (span-set-property span-processing 'face face)
	      ;; use priority API
	      (span-set-priority span-processing rank)
	      (puthash state-id span-processing tbl)))))))

(defun coq-span-color-span-processingin (xml)
  (coq-span-color-span-on-feedback
   xml
   coq-processing-span-tbl
   'processing-in
   'proof-processing-face))

(defun coq-span-color-span-incomplete (xml)
  (coq-span-color-span-on-feedback
   xml
   coq-incomplete-span-tbl
   'incomplete
   'proof-incomplete-face))

(defun coq-span-uncolor-span-on-feedback (xml tbl)
  (let* ((state-id (coq-xml-at-path xml '(feedback (state_id val))))
	 (span-colored (gethash state-id tbl)))
    ;; may get several identical feedbacks, use just first one
    (when span-colored
      (remhash state-id tbl)
      (span-delete span-colored))))

(defun coq-span-color-span-processed (xml)
  (coq-span-uncolor-span-on-feedback xml coq-processing-span-tbl)
  (coq-span-color-span-on-feedback
   xml
   coq-processing-span-tbl
   'processed
   'proof-processed-face))

(defun coq-span-uncolor-span-complete (xml)
  ;; we also get complete feedbacks for statements that dismiss last goal in proof
  ;; we ignore those
  (coq-span-uncolor-span-on-feedback xml coq-incomplete-span-tbl))

(defun coq-span-color-sent-span (span)
  (with-current-buffer proof-script-buffer
    (save-excursion
      (goto-char (span-start span))
      (skip-chars-forward " \t\n")
      (let ((span-sent (span-make (point) (span-end span)))
	    (rank (gethash proof-locked-face face-rank-tbl)))
	(span-set-property span-sent 'type 'pg-special-coloring)
	(span-set-property span-sent 'sent 't)
	(span-set-property span-sent 'face proof-locked-face)
	(span-set-priority span-sent rank)))))

(provide 'coq-span)
