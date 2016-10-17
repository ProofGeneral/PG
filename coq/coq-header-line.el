;;; coq-header-line.el -- script buffer header line (and mode line info) to track proof progress

(require 'cl-lib)

(require 'proof-faces)
(require 'coq-system)

;; colors for terminals
(defvar coq-queue-color "lightred")
(defvar coq-locked-color "lightblue")
(defvar coq-secondary-locked-color "lightgreen")
(defvar coq-processing-color "brightblue")
(defvar coq-processed-color "green")
(defvar coq-incomplete-color "blue")
(defvar coq-error-color "red")

;; make copies of PG faces so we can modify the copies without affecting the originals
;; order here is significant, want later entries have precedence
(defvar face-assocs
  `((,proof-queue-face . (coq-queue-face . ,coq-queue-color))
    (,proof-locked-face . (coq-locked-face . ,coq-locked-color))
    (,proof-secondary-locked-face . (coq-secondary-locked-face . ,coq-secondary-locked-color))
    (,proof-processing-face . (coq-processing-face . ,coq-processing-color))
    (,proof-processed-face . (coq-processed-face . ,coq-processed-color))
    (,proof-incomplete-face . (coq-incomplete-face . ,coq-incomplete-color))
    (,proof-error-face . (coq-error-face . ,coq-error-color))))

;; Table maps PG face to new face and color for TTYs
(defvar face-mapper-tbl (make-hash-table))
;; Table maps PG face to a rank governing precedence
(defvar face-rank-tbl (make-hash-table))
;; rank counter
(defvar face-rank 0)

(mapc (lambda (face-pair)
	(let ((old-face (car face-pair))
	      (new-face-color (cdr face-pair)))
	  (copy-face old-face (car new-face-color))
	  (puthash old-face new-face-color face-mapper-tbl)
	  (puthash old-face face-rank face-rank-tbl)
	  (setq face-rank (1+ face-rank))))
      face-assocs)

(defun coq-header-line-set-height ()
  "Set height of faces used in header line"
  (when coq-header-line-height
    (mapc (lambda (fce)
	    (set-face-attribute fce nil :height coq-header-line-height :strike-through "black"))
	  '(coq-queue-face
	    coq-locked-face
	    coq-secondary-locked-face
	    coq-processing-face
	    coq-incomplete-face
	    coq-error-face))))

(defun coq-header--calc-offset (pos lines cols &optional start)
  "Calculate offset into COLS for POS in a buffer of LINES; START means
this is start of offset, otherwise it's the end"
  (let* ((pos-line (line-number-at-pos pos))
	 (adjusted-line (if start (1- pos-line) pos-line)))
    (/ (* adjusted-line cols) lines)))

(defun coq-header--calc-endpoints (start-pos end-pos num-lines num-cols)
  "Given NUM-LINES in buffer and NUM-COLS in header line, calculate endpoints 
in header line clamped to number of lines contained between START-POS and END-POS buffer positions."
  (let* ((start (coq-header--calc-offset start-pos num-lines num-cols t))
	 (end (coq-header--calc-offset end-pos num-lines num-cols))
	 (start-line (line-number-at-pos start-pos))
	 (end-line (line-number-at-pos end-pos))
	 ;; lines in script
	 (endpoint-lines (1+ (- end-line start-line)))
	 ;; cols in header line
	 (start-end-cols (1+ (- end start))))
    (if (<= start-end-cols endpoint-lines)
	(cons start end)
      ;; clamp num cols in header line to num lines in script
      (let* ((half-cols (/ start-end-cols 2.0))
	     (half-lines (/ endpoint-lines 2.0))
	     (center-col (+ start half-cols))
	     (adj-start (max 1 (ceiling (- center-col half-lines))))
	     (adj-end (min num-cols (ceiling (+ center-col half-lines)))))
	(cons adj-start adj-end)))))

(defun coq-header--tiebreak-endpoints (start end num-cols)
  "Make sure entry in header line is not zero-width. START, END are 
columns in header line, NUM-COLS is number of its columns."
  (if (eq start end)
    (if (< end num-cols)
	(cons start (1+ end))
      (cons (1- start) end))
    (cons start end)))

(defvar coq-header-line-char ?\-)
(defvar coq-header-line-mouse-pointer 'hand)

(defun coq-header--colored-span-rank (sp)
  (let ((face (span-property sp 'face)))
    (gethash face face-rank-tbl)))

(defun coq-header-line--make-line (num-cols)
  (make-string num-cols coq-header-line-char))

(defvar coq-header--mode-line-face-tbl (make-hash-table :test 'equal))

(mapc (lambda (face)
	(puthash face t coq-header--mode-line-face-tbl)
	(let ((color (cdr (gethash face face-mapper-tbl))))
	  (puthash `(:background ,color) t coq-header--mode-line-face-tbl)))
      `(,proof-processing-face
	,proof-processed-face
	,proof-incomplete-face
	,proof-error-face))

(defun coq-header--mode-line-filter (elt)
  (or (eq elt 'mode-line-end-spaces)
      (and (stringp elt)
	   (let ((face (get-text-property 1 'face elt)))
	     (gethash face coq-header--mode-line-face-tbl)))))

(defun coq-header-line-update (&rest _args)
  "Update header line. _ARGS passed by some hooks, ignored"
  (if (null proof-script-buffer)
      (coq-header-line--clear-all)
    (with-current-buffer proof-script-buffer
      (let* ((num-cols (window-total-width (get-buffer-window)))
	     (num-lines
	      (save-excursion
		(goto-char (point-max))
		(skip-chars-backward "\t\n")
		(line-number-at-pos (point))))
	     (header-text (coq-header-line--make-line num-cols))
	     (all-spans (spans-all))
	     (error-count 0))
	(set-text-properties 1 num-cols `(pointer ,coq-header-line-mouse-pointer) header-text)
	;; update for queue
	(let ((queue-span (car (cl-remove-if-not (lambda (sp) (eq (span-property sp 'face) proof-queue-face)) all-spans))))
	  (when queue-span
	    (let ((start (coq-header--calc-offset (span-start queue-span) num-lines num-cols t))
		  (end (coq-header--calc-offset (span-end queue-span) num-lines num-cols)))
	      (if (display-graphic-p)
		  (set-text-properties start end `(face coq-queue-face pointer ,coq-header-line-mouse-pointer) header-text)
		(add-face-text-property start end `(:background ,coq-queue-color) nil header-text)))))
	;; update for locked region
	(let ((locked-span (car (cl-remove-if-not (lambda (sp) (eq (span-property sp 'face) proof-locked-face)) all-spans))))
	  (when locked-span
	    (let ((start (coq-header--calc-offset (span-start locked-span) num-lines num-cols t))
		  (end (coq-header--calc-offset (span-end locked-span) num-lines num-cols)))
	      (if (display-graphic-p)
		  (set-text-properties start end `(face coq-locked-face pointer ,coq-header-line-mouse-pointer) header-text)
		(add-face-text-property start end `(:background ,coq-locked-color) nil header-text)))))
	;; update for secondary locked region
	(let ((secondary-locked-span (car (cl-remove-if-not (lambda (sp) (eq (span-property sp 'face) proof-secondary-locked-face)) all-spans))))
	  (when secondary-locked-span
	    (let ((start (coq-header--calc-offset (span-start secondary-locked-span) num-lines num-cols t))
		  (end (coq-header--calc-offset (span-end secondary-locked-span) num-lines num-cols)))
	      (if (display-graphic-p)
		  (set-text-properties start end `(face coq-secondary-locked-face pointer ,coq-header-line-mouse-pointer) header-text)
		(add-face-text-property start end `(:background ,coq-secondary-locked-color) nil header-text)))))
	;; update for specially-colored spans, errors
	(let* ((vanilla-spans (cl-remove-if-not
			       (lambda (sp)
				 (eq (span-property sp 'type) 'vanilla))
			       all-spans))
	       (vanilla-count (float (length vanilla-spans)))
	       (colored-spans (cl-remove-if-not
			       (lambda (sp)
				 (let ((type (span-property sp 'type)))
				   (member type '(pg-special-coloring
						  pg-error))))
			       all-spans))
	       (sorted-spans (sort colored-spans (lambda (sp1 sp2) (< (coq-header--colored-span-rank sp1)
								      (coq-header--colored-span-rank sp2)))))
	       (processing-count 0)
	       (processed-count 0)
	       (incomplete-count 0))
	  (dolist (span sorted-spans)
	    (let* ((type (span-property span 'type))
		   (old-face (span-property span 'face))
		   (new-face-color (gethash old-face face-mapper-tbl))
		   (new-face (car new-face-color))
		   (color (cdr new-face-color))
		   (endpoints (coq-header--calc-endpoints (span-start span) (span-end span) num-lines num-cols))
		   (adj-endpoints (coq-header--tiebreak-endpoints (car endpoints) (cdr endpoints) num-cols))
		   (start (car adj-endpoints))
		   (end (cdr adj-endpoints)))
	      (pcase (span-property span 'face)
		  (`proof-processing-face (setq processing-count (1+ processing-count)))
		  (`proof-processed-face (setq processed-count (1+ processed-count)))
		  (`proof-incomplete-face (setq incomplete-count (1+ incomplete-count)))
		  (`proof-error-face (setq error-count (1+ error-count))))
	      (if (display-graphic-p)
		  (set-text-properties start end `(face ,new-face pointer ,coq-header-line-mouse-pointer) header-text)
		(add-face-text-property start end `(:background ,color) nil header-text))))
	  (setq header-line-format header-text)
	  ;; update mode line indicators
	  (when (consp mode-line-format)
	    (let ((filtered-fmt (cl-remove-if 'coq-header--mode-line-filter
					      mode-line-format)))
	      (let ((processing-pct
		     (if (<= vanilla-count 0.0)
			 (format " --- ") ; format avoids possibly duplicated interned string
		       (format " %.1f%%%% " (* (/ processing-count vanilla-count) 100.0))))
		    (processed-pct
		     (if (<= vanilla-count 0.0)
			 (format " --- ")
		       (format " %.1f%%%% "
			       (* (/ processed-count vanilla-count) 100.0))))
		    (incomplete-text
		     (if (<= vanilla-count 0.0)
			 (format " --- ")
		       (format " %d " incomplete-count)))
		    (error-text 
		     (if (<= vanilla-count 0.0)
			 (format " ---")
		       (format " %d" error-count))))
	      (if (display-graphic-p)
		  (progn
		    (add-text-properties 1 (1- (length processing-pct)) `(face ,proof-processing-face help-echo "Percentage of statements still being processed by Coq") processing-pct)
		    (add-text-properties 1 (1- (length processed-pct)) `(face ,proof-processed-face help-echo "Percentage of statements processed by Coq") processed-pct)
		    (add-text-properties 1 (1- (length incomplete-text)) `(face ,proof-incomplete-face help-echo "Number of proofs not yet kernel-checked by Coq") incomplete-text)
		    (add-text-properties 1 (length error-text) `(face ,proof-error-face help-echo "Number of errors found by Coq") error-text))
		(add-face-text-property 1 (1- (length processing-pct)) `(:background ,(cdr (gethash proof-processing-face face-mapper-tbl))) nil processing-pct)
		(add-face-text-property 1 (1- (length processed-pct)) `(:background ,(cdr (gethash proof-processed-face face-mapper-tbl))) nil processed-pct)
		(add-face-text-property 1 (1- (length incomplete-text)) `(:background ,(cdr (gethash proof-incomplete-face face-mapper-tbl))) nil incomplete-text)
		(add-face-text-property 1 (length error-text) `(:background ,(cdr (gethash proof-error-face face-mapper-tbl))) nil error-text))
	      (setq mode-line-format (reverse
				      (cons error-text
					    (cons incomplete-text
						  (cons processed-pct
							(cons processing-pct (reverse filtered-fmt)))))))))))))))

;; update header line at strategic points
(when coq-use-header-line
  (add-hook 'window-size-change-functions 'coq-header-line-update)
  (add-hook 'window-configuration-change-hook 'coq-header-line-update)
  (add-hook 'proof-server-insert-hook 'coq-header-line-update)
  (add-hook 'proof-state-change-hook 'coq-header-line-update))

(defun coq-header-line-mouse-handler ()
  (interactive)
  (let ((event (read-event)))
    (when (consp event) ; sometimes seem to get other events
      (let* ((mouse-info (car event))
	     (event-posn (cadr event))
	     (x-pos (car (posn-x-y event-posn))))
	(when (and x-pos (eq major-mode 'coq-mode) (eq mouse-info 'down-mouse-1))
	  (let* ((window-pixels (window-pixel-width (get-buffer-window)))
		 (num-lines (line-number-at-pos (point-max)))
		 (destination-line (/ (* x-pos num-lines) window-pixels)))
	    (goto-char (point-min)) (forward-line (1- destination-line))))))))

;; called by coq-mode-hook
;; can't use update function, because proof-script-buffer not yet set
(defun coq-header-line-init ()
  (let* ((num-cols (window-total-width (get-buffer-window)))
	 (num-lines
	  (save-excursion
	    (goto-char (point-max))
	    (skip-chars-backward "\t\n")
	    (line-number-at-pos (point))))
	 (header-text (coq-header-line--make-line num-cols)))
    (set-text-properties 1 num-cols `(pointer ,coq-header-line-mouse-pointer) header-text)
    (setq header-line-format header-text)
    (when (consp mode-line-format)
      (setq mode-line-format (cl-remove-if 'coq-header--mode-line-filter
					   mode-line-format)))))
    
;; we can safely clear header line for all Coq buffers after a retraction
(defun coq-header-line--clear-all ()
  (mapc
   (lambda (buf)
     (with-current-buffer buf
       (when (eq major-mode 'coq-mode)
	 (coq-header-line-init))))
   (buffer-list)))

(when coq-use-header-line
  (add-hook 'proof-deactivate-scripting-hook 'coq-header-line--clear-all))

(provide 'coq-header-line)
