;;; -*- lexical-binding: t -*-

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

;;; queries where handler just invokes default handler

(defun coq-remove-trailing-dot (s)
  "Return the string S without its trailing \".\" if any.
Return nil if S is nil."
  (if (and s (string-match "\\.\\'" s))
      (substring s 0 (- (length s) 1))
    s))

(defun coq-remove-heading-quote (s)
  "Return the string S without its heading \"\'\" if any.
Return nil if S is nil."
  (if (and s (string-match "\\`'" s))
      (substring s 1 (length s))
    s))

(defun coq-clean-id-at-point (s)
  (coq-remove-heading-quote (coq-remove-trailing-dot s)))

(defun coq-is-symbol-or-punct (c)
  "Return non nil if character C is a punctuation or a symbol constituent.
If C is nil, return nil."
  (when c
    (or (equal (char-syntax c) ?\.) (equal (char-syntax c) ?\_))))

(defun coq-grab-punctuation-left (pos)
  "Return a string made of punctuations chars found immediately before position POS."
  (let ((res nil)
        (currpos pos))
    (while (coq-is-symbol-or-punct (char-before currpos))
      (setq res (concat (char-to-string (char-before currpos)) res))
      (setq currpos (- currpos 1)))
    res))


(defun coq-grab-punctuation-right (pos)
  "Return a string made of punctuations chars found immediately after position POS."
  (let ((res nil)
        (currpos pos))
    (while (coq-is-symbol-or-punct (char-after currpos))
      (setq res (concat res (char-to-string (char-after currpos))))
      (setq currpos (+ currpos 1)))
    res))

(defun coq-notation-at-position (pos)
  "Return the notation at current point.
Support dot.notation.of.modules."
  (coq-with-altered-syntax-table
   (when (or (coq-grab-punctuation-left pos) (coq-grab-punctuation-right pos))
     (concat (coq-grab-punctuation-left pos)
             (coq-grab-punctuation-right pos)))))

(defun coq-string-starts-with-symbol (s)
  (eq 0 (string-match "\\s_" s)))

;; remove trailing dot if any.
(defun coq-id-at-point ()
  "Return the identifier at current point.
Support dot.notation.of.modules."
  (coq-with-altered-syntax-table
   (let* ((symb (cond
                 ((fboundp 'symbol-near-point) (symbol-near-point))
                 ((fboundp 'symbol-at-point) (symbol-at-point))))
          (symbclean (when symb (coq-clean-id-at-point (symbol-name symb)))))
     (when (and symb (not (zerop (length symbclean))))
       symbclean))))


(defun coq-id-or-notation-at-point ()
  (or (coq-id-at-point) (concat "\"" (coq-notation-at-position (point)) "\"")))

;; clear user *response* buffer and
;;  pass response to default response handler with nil span
(defun coq-queries-clear-and-process-response (response)
  (pg-response-clear-displays)
  (funcall 'coq-server-process-response response nil))

;; just pass response to default response handler with nil span
(defun coq-queries-process-response (response)
  (funcall 'coq-server-process-response response nil))

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

;;; interactive queries

;; query for current value of particular option
;; can't use proof-prover-last-result, because we don't know its our result
;; lexical scoping crucial here!
(defun coq-queries-test-boolean-option (opt)
  (let (options-str)
    (proof-invisible-cmd-handle-result
     (lambda ()
       (list (coq-xml-get-options) nil))
     (lambda (response) (setq options-str response)))
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
	     (result nil))
	(message "OPT-TEST: %s" opt-test)
	;; iterate through options to find one we want
	(while (and options-pairs (not result))
	  (let* ((pr (car options-pairs))
		 (opt-strs (coq-xml-at-path pr '(pair (list)))))
	    (when (equal opt-strs opt-test)
	      (message "FOUND OPTION")
	      (let ((b (coq-xml-at-path
			pr
			'(pair (list)
			       (option_state (bool) (bool) (string)
					     (option_value (bool val)))))))
		(message "OPTION FLAG IS: %s" b)
		(setq result (eq b 'true))))
	    (setq options-pairs (cdr options-pairs))))
	result))))

(defun coq-queries-ask-set-unset (ask do set-cmd unset-cmd &optional bool-opt)
  "Ask for an ident id and execute command DO in SETCMD mode.
More precisely it executes SETCMD, then DO id and finally silently UNSETCMD."
  (proof-ready-prover)
  (let* ((cmd (coq-queries-guess-or-ask-for-string ask t))
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

;; macro to build SetOption setters/unsetters
(defmacro coq-queries--mk-bool-option-setters (opt-name)
  (let* ((opt-name-str (symbol-name opt-name))
	 (maker (intern (concat "coq-queries--mk-" opt-name-str "-setters")))
 	 (setter (intern (concat "coq-queries-set-" opt-name-str)))
 	 (setter-thunk (intern (concat "coq-queries-set-" opt-name-str "-thunk")))
 	 (unsetter (intern (concat "coq-queries-unset-" opt-name-str)))
 	 (unsetter-thunk (intern (concat "coq-queries-unset-" opt-name-str "-thunk")))
	 (opt-strings (split-string (capitalize opt-name-str) "-")))
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
(coq-queries--mk-bool-option-setters printing-synth)
(coq-queries--mk-bool-option-setters printing-coercions)
(coq-queries--mk-bool-option-setters printing-wildcard)
(coq-queries--mk-bool-option-setters implicit-arguments)

;; macro for simple Query's
(defmacro coq-queries--mk-query-fun (query)
  (let* ((query-name-str (symbol-name query))
	 (query-fun (intern (concat "coq-queries-" query-name-str)))
	 (query-fun-thunk (intern (concat "coq-queries-" query-name-str "-thunk")))
	 (capped-query (replace-regexp-in-string "-" " " (capitalize query-name-str))))
    (princ (format "Defining functions via macro: %s\n" (list query-fun query-fun-thunk)))
    `(progn
       (defun ,query-fun ()
	 (list (coq-xml-query-item (concat ,capped-query ".")) nil))
       (defun ,query-fun-thunk ()
	 (lambda ()
	   (,query-fun))))))

;; call the macro
(coq-queries--mk-query-fun show-tree)
(coq-queries--mk-query-fun show-proof)
(coq-queries--mk-query-fun show-conjectures)
(coq-queries--mk-query-fun show-intros)

(defun coq-queries-print-all-thunk ()
  "Creates thunk, which when called gets context of current point in proof."
  (proof-server-send-to-prover
   (lambda ()
     ;; a bit hackish: clear out responses just before sending
     (coq-server--clear-response-buffer)
     (list (coq-xml-query-item "Print All.") nil))))

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

(defun coq-queries-ask (ask do &optional dont-guess)
  "Ask for an ident and print the corresponding term."
  (proof-ready-prover)
  (let ((cmd (format (concat do " %s.")
		     (coq-queries-guess-or-ask-for-string ask dont-guess))))
    (proof-invisible-cmd-handle-result
     (lambda ()
       (list (coq-xml-query-item cmd) nil))
     'coq-queries-clear-and-process-response)))

(provide 'coq-queries)
