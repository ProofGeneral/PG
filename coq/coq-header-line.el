;;; coq-header-line.el -- script buffer header line (and mode line info) to track proof progress

;; This file is part of Proof General.

;; Portions © Copyright 1994-2012, David Aspinall and University of Edinburgh
;; Portions © Copyright 1985-2014, Free Software Foundation, Inc
;; Portions © Copyright 2001-2006, Pierre Courtieu
;; Portions © Copyright 2010, Erik Martin-Dorel
;; Portions © Copyright 2012, Hendrik Tews
;; Portions © Copyright 2017, Clément Pit-Claudel
;; Portions © Copyright 2016-2017, Massachusetts Institute of Technology

;; Proof General is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, version 2.

;; Proof General is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with Proof General. If not, see <http://www.gnu.org/licenses/>.

(require 'cl-lib)

(require 'proof-faces)
(require 'coq-system)
(require 'coq-state-vars)

;; colors for terminals
(defvar coq-header-line-color "darkgray")
(defvar coq-queue-color "lightred")
(defvar coq-locked-color coq-queue-color)
(defvar coq-sent-color "lightblue")
(defvar coq-unprocessed-color coq-locked-color)
(defvar coq-processing-color "brightblue")
(defvar coq-processed-color "lightblue")
(defvar coq-incomplete-color "blue")
(defvar coq-secondary-locked-color "lightgreen")
(defvar coq-warning-color "yellow")
(defvar coq-error-color "darkred")

;; make copies of PG faces so we can modify the copies without affecting the originals
;; order here is significant, want later entries to have increasing precedence
(defvar face-assocs
  `((header-line . (coq-header-line-face . ,coq-header-line-color))
    (,proof-queue-face . (coq-queue-face . ,coq-queue-color))
    (,proof-locked-face . (coq-locked-face . ,coq-locked-color))
    (,proof-sent-face . (coq-sent-face . ,coq-sent-color))
    (,proof-processing-face . (coq-processing-face . ,coq-processing-color))
    (,proof-processed-face . (coq-processed-face . ,coq-processed-color))
    (,proof-unprocessed-face . (coq-unprocessed-face . ,coq-unprocessed-color))
    (,proof-incomplete-face . (coq-incomplete-face . ,coq-incomplete-color))
    (,proof-secondary-locked-face . (coq-secondary-locked-face . ,coq-secondary-locked-color))
    (,proof-warning-face . (coq-warning-face . ,coq-warning-color))
    (,proof-error-face . (coq-error-face . ,coq-error-color))))

;; Table maps PG face to new face and color for TTYs
(defvar face-mapper-tbl (make-hash-table))
;; rank counter
(defvar face-rank (1+ proof-sent-priority))

(mapc (lambda (face-pair)
	(let* ((old-face (car face-pair))
	       (new-face-color (cdr face-pair))
	       (new-face (car new-face-color)))
	  (copy-face old-face new-face)
	  (set-face-attribute new-face nil :underline "black")
	  (puthash old-face new-face-color face-mapper-tbl)
	  (puthash old-face face-rank coq-face-rank-tbl)
	  (cl-incf face-rank)))
      face-assocs)

;; save last number of incomplete proofs
(defvar coq-last-incomplete-count 0)

;; save last number of processed statements
(defvar coq-last-processed-count 0)

;; variables for line number information
(defvar coq-header-line--buff-size 0.0)
(defvar coq-header-line--num-lines 0)

;; number of chars of change before we flush line number vars
(defvar coq-header--line-number-epsilon 15)

;; if size of file has changed beyond epsilon, update line number vars
(defun coq-header--validate-line-number-vars ()
  (let ((curr-size (buffer-size)))
    (when (or (= coq-header-line--buff-size 0.0)
	      (> (abs (- coq-header-line--buff-size curr-size)) coq-header--line-number-epsilon))
      (setq coq-header-line--buff-size (float curr-size))
      (setq coq-header-line--num-lines (line-number-at-pos (point-max))))))

;; assume lines are uniform length to calculate approximate line number
(defun coq-header--get-line-number (pos)
  (if (zerop coq-header-line--buff-size)
      1
    (min coq-header-line--num-lines
	 (1+ (floor (* (/ pos coq-header-line--buff-size) coq-header-line--num-lines))))))

(defun coq-header--calc-offset (pos lines cols &optional start)
  "Calculate offset into COLS for POS in a buffer of LINES; START means
this is start of offset, otherwise it's the end"
  (let* ((pos-line (coq-header--get-line-number pos))
	 (adjusted-line (if start (1- pos-line) pos-line)))
    (/ (* adjusted-line cols) lines)))

(defun coq-header--calc-endpoints (start-pos end-pos num-lines num-cols)
  "Given NUM-LINES in buffer and NUM-COLS in header line, calculate endpoints 
in header line clamped to number of lines contained between START-POS and END-POS buffer positions."
  (let* ((start (coq-header--calc-offset start-pos num-lines num-cols t))
	 (end (coq-header--calc-offset end-pos num-lines num-cols))
	 ;; no. of lines in script
	 (endpoint-lines (max 1
			      (- (coq-header--get-line-number end-pos)
				 (coq-header--get-line-number start-pos))))
	 ;; cols in header line
	 (start-end-cols (1+ (- end start))))
    (if (<= start-end-cols endpoint-lines)
	(cons start end)
      ;; clamp num cols in header line to num lines in script
      (let* ((half-cols (/ start-end-cols 2.0))
	     (half-lines (/ endpoint-lines 2.0))
	     (center-col (+ start half-cols))
	     (adj-start (max 0 (ceiling (- center-col half-lines))))
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

(defvar coq--header-text "")

;; cached state of header line

;; (start . end) of proof-queue-span
(defvar coq-header-line--queue-cache nil)
;; (start . end) of proof-locked-span
(defvar coq-header-line--locked-cache nil)
;; (start . end) of proof-sent-span
(defvar coq-header-line--sent-cache nil)

;; flag to see if colors have been updated
(defvar coq-header-line--color-update-p nil)

;; flag to force update
(defvar coq-header-line--force-update-p nil)

;; set flag when span color changed
(defun coq-header-line-set-color-update ()
  (setq coq-header-line--color-update-p t))

(defun coq-header-line--need-update ()
  ;; tests in increasing order of expense
  (or coq-header-line--force-update-p
      coq-header-line--color-update-p
      ;; forces update after retraction
      (and proof-queue-span
	   (span-detached-p proof-queue-span)
	   proof-locked-span
	   (span-detached-p proof-locked-span))
      (and coq-header-line--queue-cache
	   proof-queue-span (span-buffer proof-queue-span)
	   (not (and (= (span-start proof-queue-span)
			(car coq-header-line--queue-cache))
		     (= (span-end proof-queue-span)
			(cdr coq-header-line--queue-cache)))))
      (and coq-header-line--locked-cache
	   proof-locked-span (span-buffer proof-locked-span)
	   (not (= (span-end proof-locked-span)
		   coq-header-line--locked-cache)))
      (and coq-header-line--sent-cache
	   proof-sent-span (span-buffer proof-sent-span)
	   (not (= (span-end proof-sent-span)
		   coq-header-line--sent-cache)))))

(defun coq-header-line--build-cache ()
  (setq coq-header-line--force-update-p nil)
  (setq coq-header-line--color-update-p nil)
  (setq coq-header-line--queue-cache
	(and proof-queue-span (span-buffer proof-queue-span)
	     (cons (span-start proof-queue-span)
		   (span-end proof-queue-span))))
  (setq coq-header-line--locked-cache
	(and proof-locked-span (span-buffer proof-locked-span)
	     (span-end proof-locked-span)))
  (setq coq-header-line--sent-cache
	(and proof-sent-span (span-buffer proof-sent-span)
	     (span-end proof-sent-span))))

(defun coq-header-line--force-update (&rest _args)
  "Forced update of header line. _ARGS passed by some hooks, ignored"
  (setq coq-header-line--force-update-p t)
  (coq-header-line-update))

;; from span, get face, color, endpoints
(defun coq-header-line--span-info (span num-cols num-lines)
  (let* ((old-face (span-property span 'face))
	 (new-face-color (gethash old-face face-mapper-tbl))
	 (new-face (car new-face-color))
	 (color (cdr new-face-color))
	 (endpoints (coq-header--calc-endpoints (span-start span) (span-end span) num-lines num-cols))
	 (adj-endpoints (coq-header--tiebreak-endpoints (car endpoints) (cdr endpoints) num-cols))
	 (start (car adj-endpoints))
	 (end (cdr adj-endpoints)))
    (cons (cons new-face color)
	  (cons start end))))

;; accessors for span info
(defun coq-header-line--span-info-face (span-info)
  (caar span-info))
(defun coq-header-line--span-info-color (span-info)
  (cdar span-info))
(defun coq-header-line--span-info-start (span-info)
  (cadr span-info))
(defun coq-header-line--span-info-end (span-info)
  (cddr span-info))

;; a "function pointer" 
(defvar coq-header-line--add-text-face-fun nil)

;; use native version, added c. Emacs 24.5
(defun coq-header-line--add-text-face-native (start end face text)
  (add-face-text-property start end face nil text))

;; roll our own version, if needed
(defun coq-header-line--add-text-face-nonnative (start end face text)
  (dotimes (offs (- end start))
    (let* ((pos (+ start offs))
	   (faces-prop (get-text-property pos 'face text))
	   (faces-prop-list
	    (if (listp faces-prop)
		faces-prop
	      (list faces-prop))))
      (if (null faces-prop-list)
	  (add-text-properties pos (1+ pos) `(face ,(list face)) text)
	(unless (equal face (car faces-prop-list))
	  (add-text-properties pos (1+ pos) `(face ,(cons face faces-prop-list)) text))))))

;; dispatch through function pointer
(defun coq-header-line--add-text-face (start end face text)
  (funcall coq-header-line--add-text-face-fun start end face text))

(defun coq-header-line-update (&rest _args)
  "Update header line. _ARGS passed by some hooks, ignored"
  (when coq-use-header-line
    (if (null proof-script-buffer)
	(coq-header-line-clear-all)
      (with-current-buffer proof-script-buffer
	;; see if we need to update anything
	(condition-case err-msg
	    (when (coq-header-line--need-update)
	      (coq-header-line--build-cache)
	      ;; check if we need to update line number information
	      (coq-header--validate-line-number-vars)
	      (let* ((num-cols (window-total-width (get-buffer-window)))
		     (num-lines 
		      (save-excursion
			(goto-char (point-max))
			(skip-chars-backward " \t\n")
			(coq-header--get-line-number (point))))
		     (header-text (progn (unless (= num-cols (length coq--header-text))
					   (setq coq--header-text (coq-header-line--make-line num-cols)))
					 coq--header-text))
		     (colored-spans nil)
		     (error-spans nil)
		     (vanilla-count 0.0) ; force float computations
		     (error-count 0)
		     (processing-count 0)
		     (processed-count 0)
		     (incomplete-count 0))
		(set-text-properties 0 num-cols `(face coq-header-line-face pointer ,coq-header-line-mouse-pointer) header-text)
		(dolist (span (spans-all))
		  (pcase (span-property span 'type)
		    (`vanilla
		     (cl-incf vanilla-count)
		     (let ((face (span-property span 'face)))
		       (when face
			 (setq colored-spans (cons span colored-spans))
			 (pcase face
			   (`proof-processing-face (cl-incf processing-count))
			   (`proof-processed-face (cl-incf processed-count))
			   ;; an incomplete Qed/Defined is also processed
			   (`proof-incomplete-face (cl-incf incomplete-count)
						   (cl-incf processed-count))))))
		    (`pg-error
		     (setq error-spans (cons span error-spans)))))
		;; update for queued region
		(when (and proof-queue-span (span-buffer proof-queue-span))
		  (let ((start (coq-header--calc-offset (span-start proof-queue-span) num-lines num-cols t))
			(end (coq-header--calc-offset (span-end proof-queue-span) num-lines num-cols)))
		    (if (display-graphic-p)
			(coq-header-line--add-text-face start end 'coq-queue-face header-text)
		      (coq-header-line--add-text-face start end `(:background ,coq-queue-color) header-text))))
		;; update for locked region
		(when (and proof-locked-span (span-buffer proof-locked-span))
		  (let ((start (coq-header--calc-offset (span-start proof-locked-span) num-lines num-cols t))
			(end (coq-header--calc-offset (span-end proof-locked-span) num-lines num-cols)))
		    (if (display-graphic-p)
			(coq-header-line--add-text-face start end 'coq-locked-face header-text)
		      (coq-header-line--add-text-face start end `(:background ,coq-locked-color) header-text))))
		;; update for sent region
		(when (and proof-sent-span (> (proof-sent-end) (point-min)))
		  (let ((start (coq-header--calc-offset (span-start proof-sent-span) num-lines num-cols t))
			(end (coq-header--calc-offset (span-end proof-sent-span) num-lines num-cols)))
		    (if (display-graphic-p)
			(coq-header-line--add-text-face start end 'coq-sent-face header-text)
		      (coq-header-line--add-text-face start end `(:background ,coq-sent-color) header-text))))
		;; update for specially-colored spans
		(let ((sorted-colored-spans (sort colored-spans (lambda (sp1 sp2) (< (span-property sp1 'priority)
										     (span-property sp2 'priority))))))
		  (dolist (span sorted-colored-spans)
		    (let* ((span-info (coq-header-line--span-info span num-cols num-lines))
			   (new-face (coq-header-line--span-info-face span-info))
			   (color (coq-header-line--span-info-color span-info))
			   (start (coq-header-line--span-info-start span-info))
			   (end (coq-header-line--span-info-end span-info)))
		      (if (display-graphic-p)
			  (coq-header-line--add-text-face start end new-face header-text)
			(coq-header-line--add-text-face start end `(:background ,color) header-text)))))
		;; update for secondary locked region
		(when (and proof-locked-secondary-span (span-buffer proof-locked-secondary-span))
		  (let ((start (coq-header--calc-offset (span-start proof-locked-secondary-span) num-lines num-cols t))
			(end (coq-header--calc-offset (span-end proof-locked-secondary-span) num-lines num-cols)))
		    (if (display-graphic-p)
			(coq-header-line--add-text-face start end 'coq-secondary-locked-face header-text)
		      (coq-header-line--add-text-face start end `(:background ,coq-secondary-locked-color) header-text))))
		;; update for error spans
		(dolist (span error-spans)
		  (cl-incf error-count)
		  (let* ((span-info (coq-header-line--span-info span num-cols num-lines))
			 (new-face (coq-header-line--span-info-face span-info))
			 (color (coq-header-line--span-info-color span-info))
			 (start (coq-header-line--span-info-start span-info))
			 (end (coq-header-line--span-info-end span-info)))
		    (if (display-graphic-p)
			(coq-header-line--add-text-face start end new-face header-text)
		      (coq-header-line--add-text-face start end `(:background ,color) header-text))))
		(setq header-line-format header-text)
		(when (and (= incomplete-count 0)
			   (> coq-last-incomplete-count 0)
			   ;; bad approximation to "user hasn't done anything since Check'ing"
			   (= coq-last-processed-count processed-count))
		  (message "All proof terms checked by the kernel"))
		(setq coq-last-incomplete-count incomplete-count)
		(setq coq-last-processed-count processed-count)
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
			   (if (and (<= vanilla-count 0.0) (<= error-count 0))
			       (format " ---")
			     (format " %d" error-count))))
		      (if (display-graphic-p)
			  (progn
			    (add-text-properties 1 (1- (length processing-pct)) `(face ,proof-processing-face help-echo "Percentage of statements still being processed by Coq") processing-pct)
			    (add-text-properties 1 (1- (length processed-pct)) `(face ,proof-processed-face help-echo "Percentage of statements processed by Coq") processed-pct)
			    (add-text-properties 1 (1- (length incomplete-text)) `(face ,proof-incomplete-face help-echo "Number of proofs not yet kernel-checked by Coq") incomplete-text)
			    (add-text-properties 1 (length error-text) `(face ,proof-error-face help-echo "Number of errors found by Coq") error-text))
			(coq-header-line--add-text-face 1 (1- (length processing-pct)) `(:background ,(cdr (gethash proof-processing-face face-mapper-tbl))) processing-pct)
			(coq-header-line--add-text-face 1 (1- (length processed-pct)) `(:background ,(cdr (gethash proof-processed-face face-mapper-tbl))) processed-pct)
			(coq-header-line--add-text-face 1 (1- (length incomplete-text)) `(:background ,(cdr (gethash proof-incomplete-face face-mapper-tbl))) incomplete-text)
			(coq-header-line--add-text-face 1 (length error-text) `(:background ,(cdr (gethash proof-error-face face-mapper-tbl))) error-text))
		      (setq mode-line-format (reverse
					      (cons error-text
						    (cons incomplete-text
							  (cons processed-pct
								(cons processing-pct (reverse filtered-fmt))))))))))
		(force-window-update proof-script-buffer)))
	  (error
	   (message "Error during header update: %s" err-msg)))))))

;; update header line at strategic points
(when coq-use-header-line
  (add-hook 'window-size-change-functions 'coq-header-line--force-update)
  (add-hook 'window-configuration-change-hook 'coq-header-line--force-update))

(defun coq-header-line-mouse-handler ()
  (interactive)
  (let ((event last-input-event))
    (when (consp event) 
      (let* ((mouse-info (car event))
	     (event-posn (cadr event))
	     (x-pos (car (posn-actual-col-row event-posn))))
	(when (and x-pos (eq major-mode 'coq-mode) (eq mouse-info 'mouse-1))
	  (let* ((window-width (window-total-width (get-buffer-window)))
		 (num-lines (coq-header--get-line-number (point-max)))
		 (destination-line (/ (* x-pos num-lines) window-width)))
	    (goto-char (point-min)) (forward-line (1- destination-line))))))))

;; how often to run header update, in seconds
(defvar coq-header-line--timer-interval 1)

(defvar coq-header-line--timer nil)

(defun coq-header-line--start-timer ()
  (unless coq-header-line--timer
    (setq coq-header-line--timer
	  (run-at-time 1 coq-header-line--timer-interval 'coq-header-line-update))))

;; called by coq-mode-hook
;; can't use update function, because proof-script-buffer not yet set
(defun coq-header-line-init ()
  (when coq-use-header-line
    (let* ((num-cols (window-total-width (get-buffer-window)))
	   (header-text (coq-header-line--make-line num-cols)))
      (set-text-properties 0 num-cols `(face coq-header-line-face pointer ,coq-header-line-mouse-pointer) header-text)
      (if (fboundp 'add-face-text-property)
	  (setq coq-header-line--add-text-face-fun 'coq-header-line--add-text-face-native)
	(setq coq-header-line--add-text-face-fun 'coq-header-line--add-text-face-nonnative))
      (setq coq--header-text header-text)
      (setq header-line-format header-text)
      (when (consp mode-line-format)
	(setq mode-line-format (cl-remove-if 'coq-header--mode-line-filter
					     mode-line-format)))
      (coq-header--validate-line-number-vars)
      (coq-header-line--start-timer))))

;; we can safely clear header line for all Coq buffers after a retraction
(defun coq-header-line-clear-all ()
  (when coq-use-header-line
    (mapc
     (lambda (buf)
       (with-current-buffer buf
	 (when (eq major-mode 'coq-mode)
	   (coq-header-line-init))))
     (buffer-list))))

(when coq-use-header-line
  ;; if process filter is taking a lot of time, 
  ;; eventually, we'll call header line update
  ;; but just do it once to avoid CPU hogging
  (setq timer-max-repeats 1))

(provide 'coq-header-line)
