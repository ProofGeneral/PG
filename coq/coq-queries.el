;; coq-queries.el --- queries and handlers for XML protocol
;; Copyright (C) TODO
;; Author: Paul Steckler
;; License:  GPL (GNU GENERAL PUBLIC LICENSE)

;; send Query messages to coqtop

;; each kind of query has (at least) two functions:
;;
;; 1) a function to send the query to coqtop
;; 2) a handler to process the result
;;
;; handlers are needed because the default processing for XML responses
;;  is not enough
;; a handler can send the response for default processing if needed

(require 'proof-script)
(require 'proof-server)

(require 'coq-syntax)
(require 'coq-xml)

;; insert intros query

;; helper
(defun coq--format-intros (output)
  "Create an “intros” form from the OUTPUT of “Show Intros”."
  (let* ((shints (replace-regexp-in-string "[\r\n ]*\\'" "" output)))
    (if (or (string= "" shints)
	    (string-match coq-error-regexp shints))
	(error "Don't know what to intro")
      (format "intros %s" shints))))

(defun coq-queries--handle-insert-intros (response)
  (with-current-buffer coq-xml-response-buffer
    (coq-xml-append-response response)
    (coq-xml-unescape-buffer)
    (let ((xml (coq-xml-get-next-xml))
	  processed)
      (while (and xml (not processed))
	(when (string-equal (coq-xml-at-path xml '(message (message_level val))) "notice")
	  (let* ((intros-xml (or
			      ;; 8.5
			      (coq-xml-at-path xml '(message (message_level) (string)))
			      ;; 8.6
			      (coq-xml-at-path xml '(message (message_level) (option) (richpp (_))))))
		 (intros (coq-xml-body1 intros-xml)))
	    (when intros
	      (setq processed t)
	      (with-current-buffer proof-script-buffer
		(indent-region (point)
			       (progn (insert (coq--format-intros intros))
				      (save-excursion
					(insert " ")
					(point))))
		;; `proof-electric-terminator' moves the point in all sorts of strange
		;; ways, so we run it last
		(let ((last-command-event ?.)) ;; Insert a dot
		  (proof-electric-terminator))))))
	(setq xml (coq-xml-get-next-xml))))))

;; query sender
(defun coq-insert-intros ()
  "Insert an intros command with names given by Show Intros.
Based on idea mentioned in Coq reference manual."
  (interactive)
  (proof-server-invisible-cmd-handle-result
   (lambda ()
     (list (coq-xml-query-item "Show Intros.") nil))
   'coq-queries--handle-insert-intros))

(provide 'coq-queries)
