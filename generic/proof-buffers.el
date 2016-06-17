;; proof-buffers.el -- buffers for both repl and server modes

(require 'pg-vars)
(require 'pg-response)
(require 'proof-config)

(defun proof-prover-set-text-representation ()
  "Adjust representation for current buffer, to match `proof-shell-unicode'."
  (unless proof-prover-unicode
    ;; Prevent interpretation of multi-byte characters.
    ;; Otherwise, chars 128-255 get remapped higher, breaking regexps
    (toggle-enable-multibyte-characters -1)))

(defun proof-prover-make-associated-buffers ()
  "Create the associated buffers and set buffer variables holding them."
  (let ((goals	"*goals*")
	(resp	"*response*")
	(trace	"*trace*")
	(thms	"*thms*"))
    (setq proof-goals-buffer    (get-buffer-create goals))
    (setq proof-response-buffer (get-buffer-create resp))

    (if (and (eq proof-interaction-mode 'server)
	     proof-server-log-traffic)
	(let ((logger (concat "*" (downcase proof-assistant) "-log*")))
	  (setq proof-server-log-buffer (get-buffer-create logger))))

    ;; currently, repl-mode only, relies on regexps
    (if proof-shell-trace-output-regexp
	(setq proof-trace-buffer (get-buffer-create trace)))
    (if proof-shell-thms-output-regexp
	(setq proof-thms-buffer (get-buffer-create thms)))

    ;; Set the special-display-regexps now we have the buffer names
    (setq pg-response-special-display-regexp
	  (proof-regexp-alt goals resp trace thms))

    (with-current-buffer proof-response-buffer
      (erase-buffer)
      (proof-prover-set-text-representation)
      (funcall proof-mode-for-response))

    (with-current-buffer proof-goals-buffer
      (erase-buffer)
      (proof-prover-set-text-representation)
      (funcall proof-mode-for-goals))

    ; repl-mode only, because depends on regexp
    (proof-with-current-buffer-if-exists proof-trace-buffer
      (erase-buffer)
      (proof-prover-set-text-representation)
      (funcall proof-mode-for-response)
      (setq pg-response-eagerly-raise nil))))

(provide 'proof-buffers)


