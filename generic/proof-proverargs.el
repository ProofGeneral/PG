;; ;;; proof-proverargs.el --- prover command-line arguments
;;

(require 'pg-user)			; proof-script, new-command-advance

(defun prover-command-line-and-names ()
  (let* ((prog-name-list1
	  (if (functionp (proof-ass-sym prog-args))
	      ;; complex assistants define <PA>-prog-args as function
	      ;; that computes the argument list.
	      (cons proof-prog-name (funcall  (proof-ass-sym prog-args)))
	    (if (proof-ass prog-args)
		;; Intermediate complex assistants set the value
		;; of <PA>-prog-args to the argument list.
		(cons proof-prog-name (proof-ass prog-args))
	      ;; Trivial assistants simply set proof-prog-name
	      (split-string proof-prog-name))))
	 (prog-name-list
	  ;; Splice in proof-rsh-command if it's non-nil
	  (if (and proof-rsh-command
		   (> (length proof-rsh-command) 0))
	      (append (split-string proof-rsh-command)
		      prog-name-list1)
	    prog-name-list1))
	 (prog-command-line (mapconcat 'identity prog-name-list " ")))
    (cons prog-name-list prog-command-line)))

(provide 'proof-proverargs)

