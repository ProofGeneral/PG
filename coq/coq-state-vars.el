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
It's the state id returned after Init command sent.")

(defun coq-reset-state-vars ()
  (setq coq-current-state-id nil
	coq-proof-state-id "1"
	coq-pending-proofs nil
	coq-current-proof-name nil
	coq-edit-id-counter 1
	coq-last-but-one-state-id "1"
	coq-last-but-one-proofnum 1
	coq-last-but-one-proofstack nil))

;; we see feedback and value-fail messages twice, once for Goal, again for Status
;; see Bug 4850
;; process each one just once, because they have effects; use table to know if they've been seen
;; to prevent this table from taking too much space, we clear it just as each Add is sent
(defvar coq-error-fail-tbl (make-hash-table :test 'equal))

;; table mapping state ids to spans created by processingin feedbacks
;; we make values weak; spans can be deleted from buffer without necessarily
;;  deleting from this table
(defvar coq-processing-span-tbl (make-hash-table :test 'equal :weakness 'value))

;; table mapping state ids to spans created by incomplete feedbacks
(defvar coq-incomplete-span-tbl (make-hash-table :test 'equal :weakness 'value))

;; table mapping state ids to spans
;; values are weak, because spans can be deleted, as on a retract
(defvar coq-span-state-id-tbl (make-hash-table :test 'equal :weakness 'key-and-value))

;; associate edit ids with spans
;; edit ids are numbers, so don't need to use 'equal as test like we did for state ids
(defvar coq-span-edit-id-tbl (make-hash-table :weakness 'value))

(defun coq-reset-tables ()
  (mapc 'clrhash
	(list coq-error-fail-tbl
	      coq-processing-span-tbl
	      coq-incomplete-span-tbl
	      coq-span-state-id-tbl
	      coq-span-edit-id-tbl)))

(provide 'coq-state-vars)

;; end coq-state-vars.el


