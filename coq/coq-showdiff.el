;; Temporary file, just for test
(defun coq-show-proof-fun ()
  (interactive)
  ;; TODO: Check if we are in a proof
  (when coq-show-proof-stepwise
    (when (eq coq-diffs 'off)
      (proof-shell-invisible-command "Show Proof." ))
    (when (eq coq-diffs 'on)
      (proof-shell-invisible-command "Show Proof Diffs."))
    (when (eq coq-diffs 'removed)
      (proof-shell-invisible-command "Show Proof Diffs removed.")))
 )
