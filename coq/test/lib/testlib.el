;;; library of helper code for PG+coq tests

(defun use-file (file)
  (switch-to-buffer (find-file file))
  (proof-activate-scripting))

(defun process-file (file)
  (use-file file)
  (proof-process-buffer))

(defun retract-file (file)
  (use-file file)
  (proof-retract-buffer))

(defun debug-msg (msg &rest args)
  (let ((str (apply 'format msg args)))
    (princ str 'external-debugging-output)
    (princ "\n" 'external-debugging-output)))

(defun verify-response (expected)
  (with-current-buffer proof-response-buffer
    (let ((got (buffer-substring-no-properties (point-min) (point-max))))
      (unless (equal expected got)
	(debug-msg "*** Contents of response buffer are NOT correct ***")
	(debug-msg "EXPECTED: %s" expected)
	(debug-msg "GOT:      %s" got)))))

(defun pause-to-refresh (&optional msg)
  ;; let processing finish
  (sleep-for 1)
  ;; make sure we can see the script
  (redisplay)
  (when msg
    (message msg))
  ;; allow human to eyeball what's happened
  (sleep-for 3))



