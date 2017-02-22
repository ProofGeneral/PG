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

(defun coq-span-color-span (span face)
  (span-set-property span 'face face)
  ;; inform header line we've updated a span color
  ;; use priority API
  (span-set-priority span (gethash face coq-face-rank-tbl))
  (coq-header-line-set-color-update))

(defun coq-span-color-span-on-feedback (xml status face &optional force-processed)
  (let* ((state-id (coq-xml-at-path xml '(feedback (state_id val))))
	 (span-with-state-id (coq-server--get-span-with-state-id state-id)))
    ;; can see feedbacks with state id not yet associated with a span
    ;; also can find a span with a state id that's been deleted from script buffer,
    ;;  but not yet garbage-collected from table
    (when (and span-with-state-id
	       (span-buffer span-with-state-id))
      (let ((curr-face (span-property span-with-state-id 'face)))
	;; don't overwrite incomplete face with processed face unless force-processed
	(unless (and (eq curr-face 'proof-incomplete-face)
		     (eq face 'proof-processed-face)
		     (not force-processed))
	  (span-set-property span-with-state-id 'pg-status status)
	  (coq-span-color-span span-with-state-id face))))))

(defun coq-span-color-span-processingin (xml)
  (coq-span-color-span-on-feedback
   xml
   'processing
   'proof-processing-face))

(defun coq-span-color-span-incomplete (xml)
  (coq-span-color-span-on-feedback
   xml
   'incomplete
   'proof-incomplete-face))

(defun coq-span-color-span-processed (xml &optional force)
  (coq-span-color-span-on-feedback
   xml
   'processed
   'proof-processed-face
   force))

(defun coq-span-color-span-complete (xml)
  ;; we also get complete feedbacks for statements that dismiss last goal in proof
  ;; we ignore those
  ;; force use of processed face
  (coq-span-color-span-processed xml t))

(defun coq-span-color-sent-span (span)
  (coq-span-color-span span 'proof-unprocessed-face))

(provide 'coq-span)
