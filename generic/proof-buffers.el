;; proof-buffers.el -- extra buffers to display to user

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
	(resp	"*response*"))

    (setq proof-goals-buffer    (get-buffer-create goals))
    (setq proof-response-buffer (get-buffer-create resp))

    (when (and (eq proof-interaction-mode 'server)
	       proof-server-log-traffic)
	(let ((logger (concat "*" (downcase proof-assistant) "-log*")))
	  (setq proof-server-log-buffer (get-buffer-create logger))
	  (with-current-buffer proof-server-log-buffer
	    (setq buffer-read-only t)
	    ;; SGML mode highlights tags
	    ;; XML mode complains too much
	    (sgml-mode))))

    ;; TODO : what is this?
    (setq pg-response-special-display-regexp
	  (proof-regexp-alt goals resp))

    (with-current-buffer proof-response-buffer
      (erase-buffer)
      (proof-prover-set-text-representation)
      (funcall proof-mode-for-response))

    (with-current-buffer proof-goals-buffer
      (erase-buffer)
      (proof-prover-set-text-representation)
      (funcall proof-mode-for-goals))))

(provide 'proof-buffers)


