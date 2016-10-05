;;; coq-header-line.el -- script buffer header line to track proof progress

(require 'proof-faces)
(require 'coq-system)

;; data used to build header line
;; 
(defvar header-line-data nil)

;; make copies of PG faces so we can modify the copies without affecting the originals

;; colors for terminals
(defvar coq-queue-color "lightred")
(defvar coq-locked-color "lightblue")
(defvar coq-secondary-locked-color "lightgreen")
(defvar coq-processing-color "darkblue")
(defvar coq-processed-color "green")
(defvar coq-incomplete-color "blue")
(defvar coq-error-color "red")

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
	     (adj-start (ceiling (- center-col half-lines)))
	     (adj-end (ceiling (+ center-col half-lines))))
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
	     (all-spans (overlays-in (point-min) (point-max))))
	(set-text-properties 1 num-cols `(pointer ,coq-header-line-mouse-pointer) header-text)
	;; update for queue
	(let ((queue-span (car (spans-filter all-spans 'face proof-queue-face))))
	  (when queue-span
	    (let ((start (coq-header--calc-offset (span-start queue-span) num-lines num-cols t))
		  (end (coq-header--calc-offset (span-end queue-span) num-lines num-cols)))
	      (if (display-graphic-p)
		  (set-text-properties start end `(face coq-queue-face pointer ,coq-header-line-mouse-pointer) header-text)
		(add-face-text-property start end `(:background ,coq-queue-color) nil header-text)))))
	;; update for locked region
	(let ((locked-span (car (spans-filter all-spans 'face proof-locked-face))))
	  (when locked-span
	    (let ((start (coq-header--calc-offset (span-start locked-span) num-lines num-cols t))
		  (end (coq-header--calc-offset (span-end locked-span) num-lines num-cols)))
	      (if (display-graphic-p)
		  (set-text-properties start end `(face coq-locked-face pointer ,coq-header-line-mouse-pointer) header-text)
		(add-face-text-property start end `(:background ,coq-locked-color) nil header-text)))))
	;; update for errors
	(let ((error-spans (spans-filter all-spans 'type 'pg-error)))
	  (dolist (span error-spans)
	    (let* ((endpoints (coq-header--calc-endpoints (span-start span) (span-end span) num-lines num-cols))
		   (adj-endpoints (coq-header--tiebreak-endpoints (car endpoints) (cdr endpoints) num-cols))
		   (start (car adj-endpoints))
		   (end (cdr adj-endpoints)))
	      (if (display-graphic-p)
		  (set-text-properties start end `(face coq-error-face pointer ,coq-header-line-mouse-pointer) header-text)
		(add-face-text-property start end `(:background ,coq-error-color) nil header-text)))))
	;; update for specially-colored spans
	(let* ((colored-spans (spans-filter all-spans 'type 'pg-special-coloring))
	       (sorted-spans (sort colored-spans (lambda (sp1 sp2) (< (coq-header--colored-span-rank sp1)
								      (coq-header--colored-span-rank sp2))))))
	  (dolist (span sorted-spans)
	    (let* ((old-face (span-property span 'face))
		   (new-face-color (gethash old-face face-mapper-tbl))
		   (new-face (car new-face-color))
		   (color (cdr new-face-color))
		   (endpoints (coq-header--calc-endpoints (span-start span) (span-end span) num-lines num-cols))
		   (adj-endpoints (coq-header--tiebreak-endpoints (car endpoints) (cdr endpoints) num-cols))
		   (start (car adj-endpoints))
		   (end (cdr adj-endpoints)))
	      (if (display-graphic-p)
		  (set-text-properties start end `(face ,new-face pointer ,coq-header-line-mouse-pointer) header-text)
		(add-face-text-property start end `(:background ,color) nil header-text)))))
	(setq header-line-format header-text)))))

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
	(when (and x-pos (eq major-mode 'coq-mode) (eq mouse-info 'double-down-mouse-1))
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
    (setq header-line-format header-text)))

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
