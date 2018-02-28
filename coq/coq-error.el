;;; coq-error.el -- use Emacs' next-error protocol

;; This file is part of Proof General.

;; Portions © Copyright 1994-2012  David Aspinall and University of Edinburgh
;; Portions © Copyright 2003, 2012, 2014  Free Software Foundation, Inc.
;; Portions © Copyright 2001-2017  Pierre Courtieu
;; Portions © Copyright 2010, 2016  Erik Martin-Dorel
;; Portions © Copyright 2011-2013, 2016-2017  Hendrik Tews
;; Portions © Copyright 2015-2017  Clément Pit-Claudel
;; Portions © Copyright 2016-2018  Massachusetts Institute of Technology

;; Proof General is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; Proof General is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with Proof General. If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:
;;

;;; Code:

(require 'span)

(require 'proof-script)

(require 'compile)
(require 'cl-lib)

;; do we need to update error buffer?
(defvar coq-error-need-update nil)

(defun coq-error-set-update ()
  (setq coq-error-need-update t))

(defvar coq-error-buffname "*coq-errors*")

(defun coq-error-update ()
  (when (and coq-error-need-update proof-script-buffer)
    (setq coq-error-need-update nil)
    (with-current-buffer proof-script-buffer
      ;; for the most part, error spans are in the sent region
      ;; but on a retraction, might be just beyond, so look at whole buffer
      (let* ((spans (spans-all))
	     (error-spans (cl-remove-if-not (lambda (sp)
					      (eq (span-property sp 'type) 'pg-error))
					    spans))
	     (sorted-error-spans (sort error-spans
				       (lambda (sp1 sp2)
					 (< (span-start sp1)
					    (span-start sp2))))))
	;; kill error buffer if not needed
	(if (null error-spans)
	    (ignore-errors
	      (kill-buffer coq-error-buffname))
	  (let ((error-buffer (get-buffer-create coq-error-buffname)))
	    (with-current-buffer error-buffer
	      (save-excursion
		(text-mode)
		(compilation-minor-mode)
		(next-error-follow-minor-mode)
		(setq buffer-read-only nil)
		(erase-buffer)
		(insert (format "### Coq error messages from script: %s ###\n\n" proof-script-buffer))
		(dolist (err-span sorted-error-spans)
		  (let ((start (span-start err-span))
			(end (span-end err-span))
			(msg (span-property err-span 'help-echo))
			line-start
			error-line)
		    (with-current-buffer proof-script-buffer
		      (save-excursion
			(goto-char start)
			(setq line-start (car (bounds-of-thing-at-point 'line)))
			(setq error-line (line-number-at-pos line-start))))
		    (goto-char (point-max))
		    (insert
		     (format "File \"%s\", line %d, characters %d-%d:\n%s\n\n"
			     proof-script-buffer
			     error-line
			     (- (span-start err-span) line-start)
			     (- (span-end err-span) line-start)
			     msg)))))
	      (setq buffer-read-only t))))))))

(defvar coq-error--timer nil)
(defvar coq-error--timer-interval 1.0) 

(defun coq-error-start-timer ()
  (unless coq-error--timer
    (setq coq-error--timer
	  (run-at-time 1 coq-error--timer-interval 'coq-error-update))))

(provide 'coq-error)
