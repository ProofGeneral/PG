;; coq-state-vars.el -- variables holding global state of the proof

;; most of these were encoded in the prompt in the old -emacs mode for coqtop
;; the current state id is returned in the last <value> in response to an "Add" command
;; the others are in the <status> in response to a "Status" command

(defvar coq-current-state-id nil)
(defvar coq-proof-state-id "1")
(defvar coq-pending-proofs nil)
(defvar coq-current-proof-name nil)
(defvar coq-edit-id-counter 1)

;; NB: state ids are strings that currently in Coq contain numbers

(defvar coq-last-but-one-state-id "1"
  "The state id we want to put in a span.
This is the prompt number given *just before* the command was sent.
This variable remembers this number and will be updated when
used see coq-set-state-number.
Initially 1 because Coq initial state has id 1.")

(defvar coq-last-but-one-proofnum 1
  "As for `coq-last-but-one-state-id' but for stack depth.")

(defvar coq-last-but-one-proofstack nil
  "As for `coq-last-but-one-state-id' but for proof stack symbols.")

(defvar coq-retract-buffer-state-id nil
  "State id to retract entire buffer.
It's the state id returned after init command sent.")

(defun coq-reset-state-vars ()
  (message "flushing state")
  (setq coq-current-state-id nil
	coq-proof-state-id "1"
	coq-pending-proofs nil
	coq-current-proof-name nil
	coq-edit-id-counter 1
	coq-last-but-one-state-id "1"
	coq-last-but-one-proofnum 1
	coq-last-but-one-proofstack nil
	coq-retract-buffer-state-id nil))

(provide 'coq-state-vars)

;; end coq-state-vars.el


