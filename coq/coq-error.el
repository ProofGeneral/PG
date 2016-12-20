;;; coq-error.el -- use Emacs' next-error protocol

(require 'span)

(require 'proof-script)

(require 'compile)
(require 'cl-lib)

;; do we need to update error buffer?
(defvar coq-error-need-update nil)

(defun coq-error-set-update ()
  (setq coq-error-need-update t))

;; list of errors
(defvar coq-error-errors nil)

(defvar coq-error-buffname "*coq-errors*")

(defun coq-error-update ()
  (when proof-script-buffer
    (with-current-buffer proof-script-buffer
      ;; for the most part, error spans are in the sent region
      ;; but on a retraction, might be just beyond, so look at whole buffer
      (let* ((spans (spans-all))
	     (error-spans (cl-remove-if-not (lambda (sp)
					      (eq (span-property sp 'type) 'pg-error))
					    spans)))
	(when (or coq-error-need-update
		  (not (equal (length error-spans)
			      (length coq-error-errors))))
	  (setq coq-error-need-update nil)
	  (setq coq-error-errors (sort error-spans
				       (lambda (sp1 sp2)
					 (< (span-start sp1)
					    (span-start sp2)))))
	  ;; kill error buffer if not needed
	  (if (null coq-error-errors)
	      (ignore-errors
		(kill-buffer coq-error-buffname))
	    (let ((error-buffer (get-buffer-create coq-error-buffname)))
	      (with-current-buffer error-buffer
		(save-excursion
		  (text-mode)
		  (compilation-minor-mode)
		  (next-error-follow-minor-mode)
		  (setq buffer-read-only nil)
		  (erase-buffer)
		  (insert (format "### Coq error messages from script: %s ###\n\n" proof-script-buffer))
		  (dolist (err-span error-spans)
		    (let ((start (span-start err-span))
			  (end (span-end err-span))
			  (msg (span-property err-span 'help-echo))
			  line-start
			  error-line)
		      (with-current-buffer proof-script-buffer
			(save-excursion
			  (goto-char start)
			  (setq line-start (car (bounds-of-thing-at-point 'line)))
			  (setq error-line (line-number-at-pos line-start))))
		      (goto-char (point-max))
		      (insert
		       (format "File \"%s\", line %d, characters %d-%d:\n%s\n\n"
			       proof-script-buffer
			       error-line
			       (- (span-start err-span) line-start)
			       (- (span-end err-span) line-start)
			       msg)))))
		(setq buffer-read-only t)))))))))

(defvar coq-error--timer nil)
(defvar coq-error--timer-interval 2) 

(defun coq-error-start-timer ()
  (unless coq-error--timer
    (setq coq-error--timer
	  (run-at-time 1 coq-error--timer-interval 'coq-error-update))))

(provide 'coq-error)
