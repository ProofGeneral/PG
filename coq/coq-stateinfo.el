;;; coq-stateinfo.el -- manage state information

(require 'proof-buffers)
(require 'coq-state-vars)

(eval-when (compile)
  (require 'span))

;; This information was encoded in the prompt using -emacs mode
;; now this information is returned by a Status call, except for the state id
(defun coq-current-proof-info ()
  (list coq-current-state-id coq-proof-state-id coq-pending-proofs
        (if coq-pending-proofs coq-current-proof-name nil)))

;; getters for proof info -- TODO use struct instead of list
(defun current-state-id-from-info (info)
  (nth 0 info))
(defun proofnum-from-info (info)
  (nth 1 info))
(defun pending-proofs-from-info (info)
  (nth 2 info))
(defun current-proof-name-from-info (info)
  (nth 3 info))

(defsubst proof-last-locked-span ()
  (with-current-buffer proof-script-buffer
    (span-at (- (proof-unprocessed-begin) 1) 'type)))

(defsubst coq-get-span-state-id (span)
  "Return the state number of the SPAN."
  (span-property span 'state-id))

(defsubst coq-get-span-proofnum (span)
  "Return the proof number of the SPAN."
  (span-property span 'proofnum))

(defsubst coq-get-span-proofstack (span)
  "Return the proof stack (names of pending proofs) of the SPAN."
  (span-property span 'proofstack))

(defsubst coq-set-span-state-id (span val)
  "Set the state number of the SPAN to VAL."
  (span-set-property span 'state-id val))

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
  "Set the last locked span's state id to the id found last time.
This id is in the *last but one* prompt (variable `coq-last-but-one-state-id').
If locked span already has a state number, then do nothing. Also updates
`coq-last-but-one-state-id' to the last state number for next time."
  (message "coq-set-state-infos")
  ;; infos = promt infos of the very last prompt
  ;; sp = last locked span, which we want to fill with prompt infos
  (let ((sp    (if proof-script-buffer (proof-last-locked-span)))
	(infos (coq-current-proof-info)))
    (setq coq-last-but-one-state-id (current-state-id-from-info infos))
    (message "set last but one state id: %s" coq-last-but-one-state-id)
    ;; set goalcmd property if this is a goal start
    ;; (ie proofstack has changed and not a save cmd)
    (progn 
      (message (format "not sp: %s" (not sp)))
      (message (format "(equal (span-property sp 'type) 'goalsave): %s" (and sp (equal (span-property sp 'type) 'goalsave))))
      (message (format "(span-property sp 'type): %s" (and sp (span-property sp 'type))))
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
    (setq coq-last-but-one-proofnum (proofnum-from-info infos))))

(provide 'coq-stateinfo)
