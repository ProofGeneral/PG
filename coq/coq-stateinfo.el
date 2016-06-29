;;; coq-stateinfo.el -- manage state information

(require 'proof-buffers)
(require 'coq-state-vars)

(eval-when (compile)
  (require 'span))

(defvar coq-last-but-one-statenum 1
  "The state number we want to put in a span.
This is the prompt number given *just before* the command was sent.
This variable remembers this number and will be updated when
used see coq-set-state-number.
Initially 1 because Coq initial state has number 1.")

(defvar coq-last-but-one-proofnum 1
  "As for `coq-last-but-one-statenum' but for stack depth.")

(defvar coq-last-but-one-proofstack '()
  "As for `coq-last-but-one-statenum' but for proof stack symbols.")

;; This information was encoded in the prompt using -emacs mode
;; now this information is returned by a Status call, except for the state id
(defun coq-current-proof-info ()
  (list coq-current-state-id coq-proof-state-id coq-pending-proofs
        (if coq-pending-proofs coq-current-proof-name nil)))

;; getters for proof info -- TODO use struct instead of list
(defun current-state-id-from-info (info)
  (nth 0 info))
(defun proof-state-id-from-info (info)
  (nth 1 info))
(defun pending-proofs-from-info (info)
  (nth 2 info))
(defun current-proof-name-from-info (info)
  (nth 3 info))

(defsubst proof-last-locked-span ()
  (with-current-buffer proof-script-buffer
    (span-at (- (proof-unprocessed-begin) 1) 'type)))

(defsubst coq-get-span-statenum (span)
  "Return the state number of the SPAN."
  (span-property span 'statenum))

(defsubst coq-get-span-proofnum (span)
  "Return the proof number of the SPAN."
  (span-property span 'proofnum))

(defsubst coq-get-span-proofstack (span)
  "Return the proof stack (names of pending proofs) of the SPAN."
  (span-property span 'proofstack))

(defsubst coq-set-span-statenum (span val)
  "Set the state number of the SPAN to VAL."
  (span-set-property span 'statenum val))

(defsubst coq-get-span-goalcmd (span)
  "Return the 'goalcmd flag of the SPAN."
  (span-property span 'goalcmd))

(defsubst coq-set-span-goalcmd (span val)
  "Set the 'goalcmd flag of the SPAN to VAL."
  (span-set-property span 'goalcmd val))

(defsubst coq-set-span-proofnum (span val)
  "Set the proof number of the SPAN to VAL."
  (span-set-property span 'proofnum val))

(defsubst coq-set-span-proofstack (span val)
  "Set the proof stack (names of pending proofs) of the SPAN to VAL."
  (span-set-property span 'proofstack val))

(defun coq-set-state-infos ()
  "Set the last locked span's state number to the number found last time.
This number is in the *last but one* prompt (variable `coq-last-but-one-statenum').
If locked span already has a state number, then do nothing. Also updates
`coq-last-but-one-statenum' to the last state number for next time."
  (message "coq-set-state-infos")
  ;; infos = promt infos of the very last prompt
  ;; sp = last locked span, which we want to fill with prompt infos
  (let ((sp    (if proof-script-buffer (proof-last-locked-span)))
	(infos (coq-current-proof-info)))
    (unless (or (not sp) (coq-get-span-statenum sp))
      (coq-set-span-statenum sp coq-last-but-one-statenum))
    (setq coq-last-but-one-statenum (current-state-id-from-info infos))
    ;; set goalcmd property if this is a goal start
    ;; (ie proofstack has changed and not a save cmd)
    '(progn 
      (message (format "not sp: %s" (not sp)))
      (message (format "(equal (span-property sp 'type) 'goalsave): %s" (equal (span-property sp 'type) 'goalsave)))
      (message (format "(span-property sp 'type): %s" (span-property sp 'type)))
      (message (format "(length (car (cdr (cdr infos)))) %s" (length (car (cdr (cdr infos))))))
      (message (format "(length coq-last-but-one-proofstack))) %s" (length coq-last-but-one-proofstack))))
    (unless
	(or (not sp) (equal (span-property sp 'type) 'goalsave)
	    (<= (length (pending-proofs-from-info infos))
		(length coq-last-but-one-proofstack)))
      (message "setting span goalcmd for: %s" sp)
      (coq-set-span-goalcmd sp t))
    ;; testing proofstack=nil is not good here because nil is the empty list OR
    ;; the no value, so we test proofnum as it is always set at the same time.
    ;; This is why this test is done before the next one (which sets proofnum)
    (unless (or (not sp) (coq-get-span-proofnum sp))
      (coq-set-span-proofstack sp coq-last-but-one-proofstack))
    (setq coq-last-but-one-proofstack (car (cdr (cdr infos))))
    (unless (or (not sp) (coq-get-span-proofnum sp))
      (coq-set-span-proofnum sp coq-last-but-one-proofnum))
    (setq coq-last-but-one-proofnum (car (cdr infos)))))

(provide 'coq-stateinfo)
