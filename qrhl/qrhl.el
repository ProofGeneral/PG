
(load-library "qrhl-input")

(defun qrhl-find-and-forget (span)
  (proof-generic-count-undos span))
  
(defvar qrhl-home (file-name-directory (directory-file-name (file-name-directory (directory-file-name (file-name-directory load-file-name))))))
  
(proof-easy-config 'qrhl "qRHL"
		   proof-prog-name (concat qrhl-home "bin/qrhl")
		   ; We need to give some option here, otherwise proof-prog-name is interpreted
		   ; as a shell command which leads to problems if the path contains spaces
		   ; (see the documentation for proof-prog-name)
           qrhl-prog-args '("--emacs")
		   proof-script-command-end-regexp "\\.[ \t]*$"
		   proof-shell-annotated-prompt-regexp "^\\(\\.\\.\\.\\|qrhl\\)> "
		   proof-script-comment-start-regexp "#"
		   proof-script-comment-end "\n"
		   proof-shell-error-regexp "\\[ERROR\\]\\|Exception"
		   proof-undo-n-times-cmd "undo %s."
		   proof-find-and-forget-fn 'qrhl-find-and-forget
		   proof-shell-start-goals-regexp "^[0-9]+ subgoals:\\|^Goal:\\|^No current goal\\.\\|^In cheat mode\\.\\|^No focused goals (use "
		   proof-shell-proof-completed-regexp "^No current goal.$"
		   proof-shell-eager-annotation-start "\\*\\*\\* "
		   proof-shell-eager-annotation-start-length 4
		   proof-no-fully-processed-buffer t
		   proof-shell-filename-escapes '(("\\\\" . "\\\\") ("\"" . "\\\""))
		   proof-shell-cd-cmd "changeDirectory \"%s\"."
		   proof-save-command-regexp "^adfuaisdfaoidsfasd" ; ProofGeneral produces warning when this is not set. But we don't want goal/save commands to be recognized because that makes ProofGeneral do an atomic undo.
		   )




; buttoning functions follow https://superuser.com/a/331896/748969
(define-button-type 'qrhl-find-file-button
  'follow-link t
  'action #'qrhl-find-file-button)

(defun qrhl-find-file-button (button)
  (find-file (buffer-substring (button-start button) (button-end button))))

(defun qrhl-buttonize-buffer ()
 "turn all include's into clickable buttons"
 (interactive)
 (remove-overlays)
 (save-excursion
  (goto-char (point-min))
  (while (re-search-forward "include\s*\"\\([^\"]+\\)\"\s*\\." nil t)
   (make-button (match-beginning 1) (match-end 1) :type 'qrhl-find-file-button))))




(add-hook 'qrhl-mode-hook
	  (lambda ()
	    (set-input-method "qrhl")
	    (set-language-environment "UTF-8")
	    (set-variable 'indent-tabs-mode nil)
	    (set-variable 'electric-indent-mode nil)
	    (qrhl-buttonize-buffer)))

(defun qr () ; Just for testing
  "Restarts the prover and then processes the buffer to the current position"
  (interactive)
  (proof-shell-exit t)
  (proof-goto-point))


(provide 'qrhl)
