;; coq-response.el

;; This file is part of Proof General.

;; Portions © Copyright 1994-2012, David Aspinall and University of Edinburgh
;; Portions © Copyright 1985-2014, Free Software Foundation, Inc
;; Portions © Copyright 2001-2006, Pierre Courtieu
;; Portions © Copyright 2010, Erik Martin-Dorel
;; Portions © Copyright 2012, Hendrik Tews
;; Portions © Copyright 2017, Clément Pit-Claudel
;; Portions © Copyright 2016-2017, Massachusetts Institute of Technology

;; Proof General is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, version 2.

;; Proof General is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with Proof General. If not, see <http://www.gnu.org/licenses/>.

(require 'proof-buffers)
(require 'proof-server)
(require 'coq-indent)
(require 'coq-header-line)
(require 'coq-error)
(require 'coq-company-compat)

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

;; *Experimental* auto shrink response buffer in three windows mode. Things get
;; a bit messed up if the response buffer is not at the right place (below
;; goals buffer) TODO: Have this linked to proof-resize-window-tofit in
;; proof-utils.el + customized by the "shrink to fit" menu entry
;;  + have it on by default when in three windows mode.
;; The philosophy is that if goals and response are on the same column, their
;; cumulated size should not change.
(defun coq-optimise-resp-windows ()
  "Resize response buffer to optimal size.
Only when three-buffer-mode is enabled."
  ;; CPC 2015-12-31: Added the check below: if the command that caused this
  ;; call was silent, we shouldn't touch the response buffer.  See GitHub
  ;; issues https://github.com/cpitclaudel/company-coq/issues/32 and
  ;; https://github.com/cpitclaudel/company-coq/issues/8.
  (unless (memq 'no-response-display proof-server-delayed-output-flags)
    ;; If there is no frame with goql+response then do nothing
    (when proof-three-window-enable 
      (let ((threeb-frames (coq-find-threeb-frames)))
	(when threeb-frames
	  (let ((pg-frame (car threeb-frames))) ; selecting one adequate frame
	    (with-selected-frame pg-frame
	      (let ((response-window (get-buffer-window proof-response-buffer t))
		    (goals-window (get-buffer-window proof-goals-buffer t)))
		(when (and response-window
			   (> (frame-height) 10))
		  (with-selected-window response-window
		    (with-current-buffer proof-response-buffer
		      (let* ((response-height (window-text-height response-window))
			     (goals-height (window-text-height goals-window))
			     (maxhgth (- (+ response-height goals-height)
					 window-min-height))
			     (nline-resp ; number of lines we want for response buffer
			      (min maxhgth (max window-min-height ; + 1 for comfort
						(+ 1 (count-lines (point-max) (point-min)))))))
			(unless (is-not-split-vertic (selected-window))
			  (shrink-window (- response-height nline-resp)))
			(goto-char (point-min))
			(recenter)))))))))))))

;; display something in response buffer
(defun coq-display-response (msg)
  (setq proof-shell-last-response-output msg)
  (pg-response-message msg)
  (coq-optimise-resp-windows-if-option))

;; given a byte offset in a multibyte string, calculate the string offset
(defun byte-offset-to-char-offset (str byte-offset)
  (let ((len (length str))
	(count 0)
	(pos 0))
    (while (and (< pos len)
		(< count byte-offset))
      (let* ((char (substring str pos (1+ pos)))
	     (unichar (string-as-unibyte char)))
	(cl-incf count (length unichar))
	(cl-incf pos)))
    pos))

;; temporarily highlight error location
(defun coq--highlight-error (span start maybe-stop)

  ;; start and stop are byte positions
  ;; if multibyte characters, those don't correspond to character positions
  ;;  and we have to calculate them

  (proof-with-current-buffer-if-exists 
   proof-script-buffer
   (let* ((raw-string (buffer-substring-no-properties (span-start span) (span-end span)))
	  (trimmed-string (replace-regexp-in-string "\\`[ \t\n]*" "" raw-string))
	  (stop (if (= 0 maybe-stop) (span-end span) maybe-stop))
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
	'self-removing t
	'priority (gethash proof-warning-face coq-face-rank-tbl))

       ;; return start of error
       err-start))))

;; handler that deletes an error span if its text is modified
(defun coq--error-span-modification-handler (span after-change-p start end &optional len)
  (when after-change-p
    (span-delete span)
    (coq-header-line-set-color-update)
    (coq-error-set-update)
    (coq-header-line-update)))

;; indelibly mark span containing an error
(defun coq-mark-error (start end error-start maybe-error-end msg &optional warp)
  ;; mark whole sentence if maybe-error-end is 0
  (let ((error-end
	 (if (= maybe-error-end 0)
	     (- end start)
	   maybe-error-end)))
    (let ((ws " \t\n"))
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
	       (spans-at-start (overlays-at (+ start start-offs)))
	       (err-spans-at-start (cl-remove-if-not (lambda (sp)
						       (eq (span-property sp 'type) 'pg-error))
						     spans-at-start)))
	  ;; if error span already there, assume it's a duplicate
	  (unless err-spans-at-start
	    (let ((error-span (span-make (+ start start-offs) (+ start end-offs))))
	      (coq-header-line-set-color-update)
	      (coq-error-set-update)
	      (span-set-property error-span 'modification-hooks (list 'coq--error-span-modification-handler))
	      (span-set-property error-span 'face proof-error-face)
	      (span-set-property error-span 'help-echo msg)
	      ;; must set priority using special call
	      (span-set-priority error-span (gethash proof-error-face coq-face-rank-tbl))
	      (span-set-property error-span 'type 'pg-error)
	      (when warp
		(goto-char (+ start start-offs)))))))
      ;; return start of error highlighting
      start)))

(defun coq-response-clear-response-buffer ()
  (pg-response-clear-displays))

(provide 'coq-response)
