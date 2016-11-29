;; coq-error -- code for managing Emacs' next-error protocol

(require 'cl-lib)

(require 'span)

;; character position of current error
(defvar coq-error-cursor nil)

;; might use this when clicking on error, for example
(defun coq-error-set-cursor (loc)
  (setq coq-error-cursor loc))

;; this function is installed as the buffer-local variable `next-error` in Coq mode
(defun coq-next-error (num-moves reset)
  (when reset
    (setq coq-error-cursor 1))
  (when (not (= num-moves 0))
    (let* ((all-spans (spans-all))
	   (error-spans (cl-remove-if-not (lambda (sp) (eq (span-property sp 'type) 'pg-error))
					  all-spans))
	   (error-locs (mapcar (lambda (sp) (span-start sp)) error-spans))
	   (sorted-locs (sort error-locs '<))
	   (locs-vector (apply 'vector sorted-locs))
	   (locs-len (length locs-vector))
	   (vec-index 0))
      (if (= locs-len 0)
	  (message "No Coq error to find")
	;; on first call, jump to first error
	(when coq-error-cursor
	  ;; find loc s.t. current pos <= loc
	  ;; linear search, number of errors should be small
	  (while (and (< vec-index locs-len)
		      (> coq-error-cursor (aref locs-vector vec-index)))
	    (cl-incf vec-index))
	  ;; make the moves
	  (setq vec-index (+ vec-index num-moves))
	  ;; wrap 
	  (setq vec-index (mod vec-index locs-len)))
	(setq coq-error-cursor (aref locs-vector vec-index))
	(goto-char coq-error-cursor)))))

(provide 'coq-error)

	  
      
	
      


