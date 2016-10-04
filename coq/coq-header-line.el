;;; coq-header-line.el -- script buffer header line to track proof progress

(require 'proof-faces)
(require 'coq-system)

;; data used to build header line
;; 
(defvar header-line-data nil)

;; make copies of PG faces so we can modify the copies without affecting the originals

(defvar face-assocs
  `((,proof-queue-face . coq-queue-face)
    (,proof-locked-face . coq-locked-face)
    (,proof-secondary-locked-face . coq-secondary-locked-face)
    (,proof-processing-face . coq-processing-face)
    (,proof-incomplete-face . coq-incomplete-face)
    (,proof-script-highlight-error-face . coq-script-highlight-error-face)))

(defvar face-mapper-tbl (make-hash-table))

(mapc (lambda (face-pair)
	(let ((old-face (car face-pair))
	      (new-face (cdr face-pair)))
	  (copy-face old-face new-face)
	  (puthash old-face new-face face-mapper-tbl)))
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
	    coq-script-highlight-error-face))))

(defun coq-header--calc-offset (pos lines cols &optional start)
  "Calculate offset into COLS for POS in a buffer of LINES; START means
this is start of offset, otherwise it's the end"
  (let* ((pos-line (line-number-at-pos pos))
	 (adjusted-line (if start (1- pos-line) pos-line)))
    (/ (* adjusted-line cols) lines)))

(defvar coq-header-line-char ?\-)
(defvar coq-header-line-mouse-pointer 'hand)

(defun coq-header-line--make-line (num-cols)
  (make-string num-cols coq-header-line-char))

(defun coq-header-line-update (&rest _args)
  (when proof-script-buffer
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
	      (set-text-properties start end `(face coq-queue-face pointer ,coq-header-line-mouse-pointer) header-text))))
	;; update for locked region
	(let ((locked-span (car (spans-filter all-spans 'face proof-locked-face))))
	  (when locked-span
	    (let ((start (coq-header--calc-offset (span-start locked-span) num-lines num-cols t))
		  (end (coq-header--calc-offset (span-end locked-span) num-lines num-cols)))
	      (set-text-properties start end `(face coq-locked-face pointer ,coq-header-line-mouse-pointer) header-text))))
	;; update for specially-colored spans
	(let ((colored-spans (spans-filter all-spans 'type 'pg-special-coloring)))
	  (dolist (span colored-spans)
	    (let* ((old-face (span-property span 'face))
		   (new-face (gethash old-face face-mapper-tbl))
		   (start (coq-header--calc-offset (span-start span) num-lines num-cols t))
		   (end (coq-header--calc-offset (span-end span) num-lines num-cols)))
	      (when (eq start end)
		(if (< end num-cols)
		    (setq end (1+ end))
		  (setq start (1- start))))
	      (set-text-properties start end `(face ,new-face pointer ,coq-header-line-mouse-pointer) header-text))))
	(setq header-line-format header-text)))))

;; update header line at strategic points
(add-hook 'window-size-change-functions 'coq-header-line-update)
(add-hook 'window-configuration-change-hook 'coq-header-line-update)
(add-hook 'proof-server-insert-hook 'coq-header-line-update)
(add-hook 'proof-state-change-hook 'coq-header-line-update)
(add-hook 'after-change-functions 'coq-header-line-update)

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

(add-hook 'proof-deactivate-scripting-hook 'coq-header-line--clear-all)

(provide 'coq-header-line)
