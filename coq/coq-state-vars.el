;; coq-state-vars.el -- variables holding global state of the proof

;; most of these were encoded in the prompt in the old -emacs mode for coqtop
;; the current state id is returned in the last <value> in response to an "Add" command
;; the others are in the <status> in response to a "Status" command

(defvar coq-current-state-id 0)
(defvar coq-proof-state-id 0)
(defvar coq-pending-proofs nil)
(defvar coq-current-proof-name nil)

(provide 'coq-state-vars)

;; end coq-state-vars.el


