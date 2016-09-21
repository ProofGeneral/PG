;;; coq-tq.el --- utility to maintain a transaction queue

;; modified version of tq.el from Emacs distribution

;; TODO mention returning current call
;;      handlers for partial responses

;; The main modification is that queue may contain elements that are thunks, as 
;; well as strings. That allows us to invoke the thunk just before sending 
;; its results to the process, which allows capturing state variables that 
;; may have been changed by previous responses, in particular the state-id 
;; in a <value> response from coqtop. The thunk returns a list consisting 
;; of the span associated with a Coq sentence and the sentence wrapped 
;; in XML, suitable to send to coqtop. We return the span along with the 
;; coqtop response so we can set the state id in the span metadata.
;; 

;; Another modification is that we can log the strings sent to the process.
;; That way, we see the correct order of calls and responses, which we would 
;; not see if we logged the sent strings at the time of queueing.

;; When creating the transaction queue, we pass a handler for out-of-band data.

;; Finally, the enqueue function does not take the optional delay-sending argument.
;; We always delay sending until the last response has been received.


;; ****************************************************************************

;; Copyright (C) 1985-1987, 1992, 2001-2013 Free Software Foundation,
;; Inc.

;; Author: Scott Draves <spot@cs.cmu.edu>
;; Maintainer: FSF
;; Adapted-By: ESR
;; Keywords: extensions

;; This file is part of GNU Emacs.

;; GNU Emacs is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; GNU Emacs is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; This file manages receiving a stream asynchronously, parsing it
;; into transactions, and then calling the associated handler function
;; upon the completion of each transaction.

;; Our basic structure is the queue/process/buffer triple.  Each entry
;; of the queue part is a list of question, regexp, closure, and
;; function that is consed to the last element.

;; A transaction queue may be created by calling `tq-create'.

;; A request may be added to the queue by calling `tq-enqueue'.  If
;; the `delay-question' argument is non-nil, we will wait to send the
;; question to the process until it has finished sending other input.
;; Otherwise, once a request is enqueued, we send the given question
;; immediately to the process.

;; We then buffer bytes from the process until we see the regexp that
;; was provided in the call to `tq-enqueue'.  Then we call the
;; provided function with the closure and the collected bytes.  If we
;; have indicated that the question from the next transaction was not
;; sent immediately, send it at this point, awaiting the response.

(require 'proof-config)
(require 'proof-utils)
(require 'proof-resolver)
(require 'coq-state-vars)

;;; Code:

;;; Accessors

;; This part looks like ((queue . (end-regexp . (other-regexp . (process . buffer)))) . (response-complete . (call . span)))

(defun tq-qpb                 (tq) (car tq))
(defun tq-queue               (tq) (car (tq-qpb tq)))
(defun tq-end-regexp          (tq) (car (cdr (tq-qpb tq))))
(defun tq-other-regexp        (tq) (car (cdr (cdr (tq-qpb tq)))))
(defun tq-process             (tq) (car (cdr (cdr (cdr (tq-qpb tq))))))
(defun tq-buffer              (tq) (cdr (cdr (cdr (cdr (tq-qpb tq))))))

(defun tq-state-items         (tq) (cdr tq))
(defun tq-response-complete   (tq) (car (tq-state-items tq)))
(defun tq-call                (tq) (car (cdr (tq-state-items tq))))
(defun tq-span                (tq) (cdr (cdr (tq-state-items tq))))

(defun tq-set-complete        (tq) (setcar (tq-state-items tq) t))

;; The structure of `queue' is as follows
;; ((question closure . fn)
;;  <other queue entries>)
;; question: string to send to the process
(defun tq-queue-head-question (tq) (car (car (tq-queue tq))))
;; end-regexp: regular expression that matches the end of a response from
;; the process
(defun tq-queue-head-closure   (tq) (car (cdr (car (tq-queue tq)))))
;; other-regexp: regular expression that matches a partial response from
;; the process
(defun tq-queue-head-fn        (tq) (cdr (cdr (car (tq-queue tq)))))
;; closure: additional data to pass to the function

;; Determine whether queue is empty
(defun tq-queue-empty         (tq) (not (tq-queue tq)))

;;; added for Coq

;; handler for out-of-band responses from coqtop
(defvar tq--oob-handler nil)

(defun tq-maybe-log (src str)
  (when proof-server-log-traffic
    (proof-server-log src str)))

(defun tq-log-and-send (tq question)
  (let* ((str-and-span 
	  (cond 
	   ((stringp question) (list question nil))
	   ((symbolp question) (list (symbol-value question) nil))
	   ((functionp question) (funcall question))
	   (t (error "tq-queue-pop: expected string or function, got %s of type %s" question (type-of question)))))
	 (str (car str-and-span))
	 (span (cadr str-and-span)))
    (tq-maybe-log "emacs" str)
    ;; call to be returned with coqtop response
    (setcdr tq (cons nil       ; not complete
		     (cons str ; call
			   span)))
    ;; associate edit id with this span
    (when span
      (puthash coq-edit-id-counter span coq-span-edit-id-tbl))
    (process-send-string (tq-process tq) str)))

(defun tq-flush (tq)
  ;; flush queue 
  (setcar (tq-qpb tq) nil)
  ;; reset complete flag
  (tq-set-complete tq)
  ;; remove any parts of any responses
  (with-current-buffer (tq-buffer tq)
    (erase-buffer)))

;;; Core functionality

;;;###autoload
(defun tq-create (process oob-handler end-regexp other-regexp)
  "Create and return a transaction queue communicating with PROCESS.
PROCESS should be a subprocess capable of sending and receiving
streams of bytes.  It may be a local process, or it may be connected
to a tcp server on another machine. The OOB-HANDLER handles responses
from the PROCESS that are not transactional."
  (let* ((qpb (cons nil
		    (cons end-regexp
			  (cons other-regexp
				(cons process
				      (generate-new-buffer
				       (concat " tq-temp-"
					       (process-name process))))))))
	 (state (cons nil (cons nil nil)))
	 (tq (cons qpb state)))
    (buffer-disable-undo (tq-buffer tq))
    (setq tq--oob-handler oob-handler)
    (set-process-filter process
			`(lambda (proc string)
			   (tq-filter ',tq string)))
    tq))

(defun tq-queue-add (tq question closure fn)
  (let ((new-item (cons question (cons closure fn))))
    (setcar (tq-qpb tq) 
	    (nconc (tq-queue tq)
		   (list new-item)))
    'ok))

(defun tq-queue-pop (tq)
  (let ((queue-items (tq-queue tq)))
    (setcar (tq-qpb tq) (cdr queue-items)))
  (let ((question (tq-queue-head-question tq)))
    (condition-case nil
	(when question
	  (tq-log-and-send tq question))
      (error nil)))
  (null (car tq)))

(defun tq-enqueue (tq question closure fn)
  "Add a transaction to transaction queue TQ.
This sends the string QUESTION to the process that TQ communicates with.

When the corresponding answer comes back, we call FN with two
arguments: CLOSURE, which may contain additional data that FN
needs, and the answer to the question.

END-REGEXP is a regular expression to match the entire answer;
that's how we tell where the answer ends.

OTHER-REGEXP is a regular expression to match a partial answer, 
which we can process, without allowing the next item in the queue
to be sent."
  (let ((sendp (not (tq-queue tq)))) ;; always delay sending
    (tq-queue-add tq (unless sendp question) closure fn)
    (when sendp
      (tq-log-and-send tq question))))

(defun tq-close (tq)
  "Shut down transaction queue TQ, terminating the process."
  (delete-process (tq-process tq))
  (kill-buffer (tq-buffer tq)))

(defun tq-filter (tq string)
  "Append STRING to the TQ's buffer; then process the new data."
  (let ((buffer (tq-buffer tq)))
    (when (buffer-live-p buffer)
      (with-current-buffer buffer
	(goto-char (point-max))
	(insert string)
	(tq-process-buffer tq)))))

(defun tq-process-buffer (tq)
  "Check TQ's buffer for the regexp at the head of the queue."
  (let ((buffer (tq-buffer tq)))
    (when (buffer-live-p buffer)
      (set-buffer buffer)
      (if (= 0 (buffer-size)) ()
	(if (tq-queue-empty tq)
	    ;; feedbacks not prompted by call
	    ;; original code put response here in a *spurious* buffer
	    (let ((oob-response (buffer-string)))
	      (when (search-forward (tq-end-regexp tq) nil t) 
		(tq-set-complete tq))
	      (tq-maybe-log "coqtop-oob" oob-response)
	      (delete-region (point-min) (point-max))
	      (funcall tq--oob-handler oob-response (tq-call tq) (tq-span tq)))
	  ;; multiple else-forms
	  (goto-char (point-min))
	  (cond
	   ;; complete response
	   ;; can safely pop item from queue and send it
	   ((re-search-forward (tq-end-regexp tq) nil t) 
	    (tq-set-complete tq)
	    (let ((answer (buffer-substring (point-min) (point))))
	      (delete-region (point-min) (point))
	      (unwind-protect
		  (condition-case err
		      (progn
			(tq-maybe-log "coqtop" answer)
			(funcall (tq-queue-head-fn tq)
				 (tq-queue-head-closure tq)
				 answer 
				 (tq-call tq)
				 (tq-span tq)))
		    (error (proof-debug-message "Error when processing complete Coq response: %s, response was: \"%s\"" err answer)))
		(tq-queue-pop tq))
	      (tq-process-buffer tq)))
	   ;; partial response
	   ;; don't pop item from queue
	   ((re-search-forward (tq-other-regexp tq) nil t)
	    (let ((answer (buffer-substring (point-min) (point))))
	      (delete-region (point-min) (point))
	      (condition-case err
		  (progn
		    (tq-maybe-log "coqtop" answer)
		    (funcall (tq-queue-head-fn tq)
			     (tq-queue-head-closure tq)
			     answer 
			     (tq-call tq)
			     (tq-span tq)))
		(error (proof-debug-message "Error when processing partial Coq response: %s, response was: \"%s\"" err answer)))
	      (tq-process-buffer tq)))))))))

(provide 'coq-tq)

;;; coq-tq.el ends here
