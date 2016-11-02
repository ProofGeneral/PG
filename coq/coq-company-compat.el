;;; coq-company-compat.el

;; Compatibility file for use with company-coq

;;; DECLARATIONS

;; variables

(defvar proof-shell-last-goals-output "")
(defvar proof-shell-last-response-output "")

;;; ALIASES

;; variables

(defvaralias 'proof-shell-proof-completed 'proof-prover-proof-completed)

;; functions

(defalias 'proof-shell-invisible-command 'proof-prover-invisible-command)
(defalias 'proof-shell-available-p 'proof-prover-available-p)
(defalias 'proof-shell-live-buffer 'proof-prover-live-buffer)
(defalias 'proof-shell-ready-prover 'proof-prover-ready-prover)

(provide 'coq-company-compat)




