;; coq-response.el

(require 'proof-buffers)
(require 'proof-server)
(require 'coq-indent)
(require 'coq-header-line)

(defvar coq-time-commands nil)        ; defpacustom
(defconst coq--time-prefix "Time "
  "Coq command prefix for displaying timing information.")

(defcustom coq-optimise-resp-windows-enable t
  "If non-nil (default) resize vertically response windw after each command."
  :type 'boolean
  :group 'coq)

(defun is-not-split-vertic (selected-window)
  (<= (- (frame-height) (window-height)) 2))

(defun coq-find-threeb-frames ()
  "Return a list of frames displaying both response and goals buffers."
  (let* ((wins-resp (get-buffer-window-list proof-response-buffer nil t))
         (wins-gls (get-buffer-window-list proof-goals-buffer nil t))
         (frame-resp (cl-mapcar 'window-frame wins-resp))
         (frame-gls (cl-mapcar 'window-frame wins-gls)))
    (filtered-frame-list (lambda (x) (and (member x frame-resp) (member x frame-gls))))))

(defun coq-optimise-resp-windows-if-option ()
  (when coq-optimise-resp-windows-enable (coq-optimise-resp-windows)))

(defun coq-optimise-resp-windows ()
  "Resize response buffer to optimal size.
Only when three-buffer-mode is enabled."
  ;; CPC 2015-12-31: Added the check below: if the command that caused this
  ;; call was silent, we shouldn't touch the response buffer.  See GitHub
  ;; issues https://github.com/cpitclaudel/company-coq/issues/32 and
  ;; https://github.com/cpitclaudel/company-coq/issues/8.
  (when proof-response-buffer
    (unless (memq 'no-response-display proof-server-delayed-output-flags)
      ;; If there is no frame with goql+response then do nothing
      (when (and proof-three-window-enable (coq-find-threeb-frames))
	(let ((pg-frame (car (coq-find-threeb-frames)))) ; selecting one adequat frame
	  (with-selected-frame pg-frame
	    (when (and (> (frame-height) 10)
		       (get-buffer-window proof-response-buffer))
	      (let ((maxhgth
		     (- (+ (with-selected-window (get-buffer-window proof-goals-buffer t) (window-text-height))
			   (with-selected-window (get-buffer-window proof-response-buffer t) (window-text-height)))
			window-min-height))
		    hgt-resp nline-resp)
		(with-selected-window (get-buffer-window proof-response-buffer)
		  (setq hgt-resp (window-text-height))
		  (with-current-buffer proof-response-buffer
		    (setq nline-resp ; number of lines we want for response buffer
			  (min maxhgth (max window-min-height ; + 1 for comfort
					    (+ 1 (count-lines (point-max) (point-min)))))))
		  (unless (is-not-split-vertic (selected-window))
		    (shrink-window (- hgt-resp nline-resp)))
		  (with-current-buffer proof-response-buffer
		    (goto-char (point-min))
		    (recenter)))))))))))

;; display something in response buffer
(defun coq--display-response (msg)
  (pg-response-message msg)
  (coq-optimise-resp-windows-if-option))

;; given a byte offset in a multibyte string, calculate the string offset
;; TODO find a home for this function
(defun byte-offset-to-char-offset (str byte-offset)
  (let ((len (length str))
	(count 0)
	(pos 0))
    (while (and (< pos len) (< count byte-offset))
      (let* ((char (substring str pos (1+ pos)))
	     (unichar (string-as-unibyte char)))
	(setq count (+ count (length unichar)))
	(setq pos (1+ pos))))
    pos))

;; temporarily highlight error location
(defun coq--highlight-error (span start stop)

  ;; start and stop are byte positions
  ;; if multibyte characters, those don't correspond to character positions
  ;;  and we have to calculate them

  (proof-with-current-buffer-if-exists 
   proof-script-buffer
   (let* ((raw-string (buffer-substring-no-properties (span-start span) (span-end span)))
	  (trimmed-string (replace-regexp-in-string "\\`[ \t\n]*" "" raw-string))
	  (char-start (byte-offset-to-char-offset trimmed-string start))
	  (char-stop (byte-offset-to-char-offset trimmed-string stop))
	  (len0 (- char-stop char-start))
	  (time-offset (if coq-time-commands (length coq--time-prefix) 0))
	  (len1 (- len0 time-offset)))

     ;; beginning of line
     (goto-char (span-start span))
     (coq-find-real-start)

     ;; go to error start
     (goto-char (+ (point) char-start))

     (let ((err-start (point)))
       (span-make-self-removing-span 
	;; endpoints
	err-start (+ err-start len1)
	;; properties
	'face 'proof-warning-face
	'self-removing t)
       ;; return start of error
       err-start))))

;; handler that deletes an error span if its text is modified
(defun coq--error-span-modification-handler (span after-change-p start end &optional len)
  (when after-change-p
    (span-delete span)
    (coq-header-line-update)))

;; indelibly mark span containing an error
(defun coq-mark-error (span error-start error-end msg)
  (when (and span (span-buffer span))
    (let ((start (span-start span))
	  (end (span-end span))
	  (ws " \t\n"))
      (with-current-buffer proof-script-buffer 
	(save-excursion
	  (goto-char start)
	  (skip-chars-forward ws end)
	  (setq start (point))
	  (goto-char end)
	  (skip-chars-backward ws start)
	  (setq end (point)))
	(let* ((trimmed-string (buffer-substring start end))
	       (start-offs (byte-offset-to-char-offset trimmed-string error-start))
	       (end-offs (byte-offset-to-char-offset trimmed-string error-end))
	       (error-span (span-make (+ start start-offs) (+ start end-offs))))
	  (coq-header-line-set-color-update)
	  (span-set-property error-span 'modification-hooks (list 'coq--error-span-modification-handler))
	  (span-set-property error-span 'face proof-error-face)
	  (span-set-property error-span 'help-echo msg)
	  ;; must set priority using special call
	  (span-set-priority error-span (gethash proof-error-face face-rank-tbl))
	  (span-set-property error-span 'type 'pg-error)))
      ;; return start of error highlighting
      start)))

(provide 'coq-response)
