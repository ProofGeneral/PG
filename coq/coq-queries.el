;;; -*- lexical-binding: t -*-

;; coq-queries.el --- queries and handlers for XML protocol
;; Copyright (C) TODO
;; Author: Paul Steckler
;; License:  GPL (GNU GENERAL PUBLIC LICENSE)

;; send Query messages to coqtop, use handler to process response

;; handlers are needed because the default processing for XML responses
;;  is not enough
;; a handler can daisy-chain the response for default processing if needed

(require 'proof-script)
(require 'proof-server)

(require 'coq-syntax)
(require 'coq-parsing)
(require 'coq-xml)

;; forward declarations to silence warnings
(declare-function coq-server-process-response "coq-server.el")
(declare-function coq-server--clear-response-buffer "coq-server.el")

;; some handlers

;; clear user *response* buffer and
;;  pass response to default response handler
(defun coq-queries-clear-and-process-response (response call span)
  (pg-response-clear-displays)
  (funcall 'coq-server-process-response response call span))

;; just pass response to default response handler
(defun coq-queries-process-response (response call span)
  (funcall 'coq-server-process-response response call span))

;; extract string from message
;; used for build-match, insert-intros, for example
(defun coq-queries-get-message-string (response)
  (with-current-buffer coq-xml-response-buffer
    (coq-xml-append-response response)
    (coq-xml-unescape-buffer)
    (let ((xml (coq-xml-get-next-xml))
	  message)
      (while (and xml (not message))
	(when (string-equal (coq-xml-at-path xml '(message (message_level val))) "notice")
	  (let* ((message-xml (or
			      ;; 8.5
			      (coq-xml-at-path xml '(message (message_level) (string)))
			      ;; 8.6
			      (coq-xml-at-path xml '(message (message_level) (option) (richpp (_)))))))
	    (when message-xml
	      (setq message (coq-xml-body1 message-xml)))))
	(setq xml (coq-xml-get-next-xml)))
      message)))

;; query for current value of particular option
;; lexical scoping crucial here!
(defun coq-queries-test-boolean-option (opt)
  (let (options-str)
    (proof-invisible-cmd-handle-result
     (lambda ()
       (list (coq-xml-get-options) nil))
     (lambda (response _call _span) (setq options-str response)))
    ;; wait for response
    (while (not options-str)
      (accept-process-output proof-server-process 0.1))
    ;; use usual response buffer
    (with-current-buffer coq-xml-response-buffer
      (coq-xml-append-response options-str)
      (coq-xml-unescape-buffer)
      (let* ((xml (coq-xml-get-next-xml))
	     (options (coq-xml-at-path xml '(value (list))))
	     (options-pairs (coq-xml-body options))
	     (opt-test (append '(list nil)
			       (mapcar
				(lambda (s) `(string nil ,s))
				opt)))
	     result)
	;; iterate through options to find one we want
	(while (and options-pairs (not result))
	  (let* ((pr (car options-pairs))
		 (opt-strs (coq-xml-at-path pr '(pair (list)))))
	    (when (equal opt-strs opt-test)
	      (let ((b (coq-xml-at-path
			pr
			'(pair (list)
			       (option_state (bool) (bool) (string)
					     (option_value (bool val)))))))
		(setq result b)))
	    (setq options-pairs (cdr options-pairs))))
	(eq result 'true)))))

;; macro to build SetOption setters/unsetters
(defmacro coq-queries--mk-bool-option-setters (opt-name)
  (let* ((opt-name-str (symbol-name opt-name))
	 (maker (intern (concat "coq-queries--mk-" opt-name-str "-setters")))
 	 (setter (intern (concat "coq-queries-set-" opt-name-str)))
 	 (setter-thunk (intern (concat "coq-queries-set-" opt-name-str "-thunk")))
 	 (unsetter (intern (concat "coq-queries-unset-" opt-name-str)))
 	 (unsetter-thunk (intern (concat "coq-queries-unset-" opt-name-str "-thunk")))
	 (opt-strings (split-string (capitalize opt-name-str) "-")))
    ;; compile-time message
    (princ (format "Defining functions via macro: %s\n"
	     (list setter setter-thunk unsetter unsetter-thunk)))
    `(progn
       (defun ,maker (b)
       	 (lambda ()
       	   (list (coq-xml-set-options 
       		  ;; tricky!
       		  ;; using just unquote treats this as an application
       		  (list ,@opt-strings) 
       		  (coq-xml-option_value 
       		   '((val . boolvalue))
       		   (coq-xml-bool b)))
       		 nil)))
       (defun ,setter ()
	 (,maker 'true))
       (defun ,setter-thunk ()
	 (,setter))
       (defun ,unsetter ()
	 (,maker 'false))
       (defun ,unsetter-thunk ()
	 (,unsetter)))))

;; call the macro
(coq-queries--mk-bool-option-setters printing-all)
(coq-queries--mk-bool-option-setters printing-implicit)
(coq-queries--mk-bool-option-setters printing-coercions)
(coq-queries--mk-bool-option-setters printing-universes)
(coq-queries--mk-bool-option-setters printing-synth)
(coq-queries--mk-bool-option-setters printing-wildcard)

;; macro to build SetOption int setters/unsetters
(defmacro coq-queries--mk-int-option-setters (opt-name)
  (let* ((opt-name-str (symbol-name opt-name))
 	 (setter (intern (concat "coq-queries-set-" opt-name-str)))
	 (opt-strings (split-string (capitalize opt-name-str) "-")))
    ;; compile-time message
    (princ (format "Defining function via macro: %s\n" setter))
    `(progn
       (defun ,setter (n)
       	 (lambda ()
       	   (list (coq-xml-set-options 
       		  (list ,@opt-strings) 
       		  (coq-xml-option_value 
       		   '((val . intvalue))
		   (coq-xml-option '((val . some))
				   (coq-xml-int n))))
       		 nil))))))

;; call the macro
(coq-queries--mk-int-option-setters printing-depth)
(coq-queries--mk-int-option-setters printing-width)

;; macro for simple Query's
(defmacro coq-queries--mk-query-fun (query)
  (let* ((query-name-str (symbol-name query))
	 (query-fun (intern (concat "coq-queries-" query-name-str)))
	 (query-fun-thunk (intern (concat "coq-queries-" query-name-str "-thunk")))
	 (capped-query (replace-regexp-in-string "-" " " (capitalize query-name-str))))
    ;; compile-time message
    (princ (format "Defining functions via macro: %s\n" (list query-fun query-fun-thunk)))
    `(progn
       (defun ,query-fun ()
	 (coq-server--clear-response-buffer)
	 (list (coq-xml-query-item (concat ,capped-query ".")) nil))
       (defun ,query-fun-thunk ()
	 (lambda ()
	   (,query-fun))))))

;; call the macro
(coq-queries--mk-query-fun print-all)
(coq-queries--mk-query-fun print-hint)
(coq-queries--mk-query-fun print-scopes)
(coq-queries--mk-query-fun print-visibility)
(coq-queries--mk-query-fun pwd)
(coq-queries--mk-query-fun show-conjectures)
(coq-queries--mk-query-fun show-intros)
(coq-queries--mk-query-fun show-proof)
(coq-queries--mk-query-fun show-tree)

;; macro to build Query string commands
(defmacro coq-queries--mk-string-query-fun (query)
  (let* ((query-name-str (symbol-name query))
	 (query-fun (intern (concat "coq-queries-" query-name-str)))
	 (capped-query (replace-regexp-in-string "-" " " (capitalize query-name-str))))
    ;; compile-time message
    (princ (format "Defining function via macro: %s\n" query-fun))
    `(progn
       (defun ,query-fun (s)
       	 (lambda ()
       	   (list (coq-xml-query-item
       		  (concat ,capped-query " \"" s "\" ."))
       		 nil))))))

;; interactive queries

(defun coq-queries-guess-or-ask-for-string (s &optional dontguess)
  "Asks for a coq identifier with message S.
If DONTGUESS is non-nil, propose a default value as follows:

If region is active, suggest its content as default value.

Otherwise suggest the identifier at point, if any."
  (let* ((guess
          (cond
           (dontguess nil)
           ((use-region-p)
            (buffer-substring-no-properties (region-beginning) (region-end)))
           (t (coq-id-or-notation-at-point)))))
    (read-string
     (if guess (concat s " (default " guess "): ") (concat s ": "))
     nil 'proof-minibuffer-history guess)))

(defun coq-queries-ask-set-unset (ask do set-cmd unset-cmd &optional bool-opt)
  "Ask for an ident id and execute command DO in SETCMD mode.
More precisely it executes SET-CMD, then DO, finally UNSETCMD."
  (proof-ready-prover)
  (let* ((cmd (coq-queries-guess-or-ask-for-string ask t))
	 ;; expensive: get all options, find the one we're interested in
	 (flag-is-set (and bool-opt (coq-queries-test-boolean-option bool-opt))))
    (unless flag-is-set
      (proof-invisible-cmd-handle-result
       set-cmd
       'coq-queries-process-response))
    (proof-invisible-cmd-handle-result
     (lambda ()
       (list (coq-xml-query-item (format (concat do " %s .") cmd)) nil))
     'coq-queries-clear-and-process-response)
    (unless flag-is-set
      (proof-invisible-cmd-handle-result
       unset-cmd
       'coq-queries-process-response))))

(defun coq-queries-ask-show-all (ask do)
  "Ask for an ident and print the corresponding term."
  (coq-queries-ask-set-unset
   ask do
   ;; setter
   (coq-queries-set-printing-all)
   ;; unsetter
   (coq-queries-unset-printing-all)
   ;; option to test
   '("Printing" "All")))

(defun coq-queries-ask-show-implicits (ask do)
  "Ask for an ident and print the corresponding term."
  (coq-queries-ask-set-unset
   ask do
   ;; setter
   (coq-queries-set-printing-implicit)
   ;; unsetter
   (coq-queries-unset-printing-implicit)
   ;; option to test
   '("Printing" "Implicits")))

(defun coq-queries-ask (ask do &optional dont-guess)
  "Ask for an ident and print the corresponding term."
  (let ((cmd (format (concat do " %s .")
		     (coq-queries-guess-or-ask-for-string ask dont-guess))))
    (proof-invisible-cmd-handle-result
     (lambda ()
       (list (coq-xml-query-item cmd) nil))
     'coq-queries-clear-and-process-response)))

(provide 'coq-queries)
