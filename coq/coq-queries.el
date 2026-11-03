;;; coq-queries.el --- Queries to Coq: About, Check, Search etc.  -*- lexical-binding:t -*-

;; This file is part of Proof General.

;; Portions © Copyright 1994-2012  David Aspinall and University of Edinburgh
;; Portions © Copyright 2003-2021  Free Software Foundation, Inc.
;; Portions © Copyright 2001-2026  Pierre Courtieu
;; Portions © Copyright 2010, 2016  Erik Martin-Dorel
;; Portions © Copyright 2011-2013, 2016-2017  Hendrik Tews
;; Portions © Copyright 2015-2017  Clément Pit-Claudel

;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:
;;
;; The query commands: they ask for an identifier or a term, guessed
;; from point, and send a query about it to Coq, like About, Check,
;; Print, Search or Locate.
;;
;; They are common to all Coq backends: they run the query with
;; `coq-run-query', which calls `coq-query-function', set by the
;; backend.  Their keys are in `coq-mode-map'.

;;; Code:

(require 'subr-x)                       ; `string-trim-right'
(require 'coq-syntax)                   ; `coq-with-altered-syntax-table'
(require 'coq-db)                       ; `coq-build-command-from-db'

(defvar proof-minibuffer-history)
(defvar coq-highlight-hyps-cited-in-response)
(declare-function coq-highlight-hyps-cited-in-response "coq")

(defvar-local coq-query-function nil
  "Function running a query, set by each Coq backend.
It is called with the arguments COMMAND FLAGS THEN.  COMMAND is the
query, without its final dot, for instance \"About nat\".  FLAGS is a
list of commands setting boolean printing flags for this query only,
for instance (\"Set Printing All\"), the backend undoes them after the
query.  THEN is nil or a function, called without argument once the
answer is displayed.")

(defun coq-run-query (command &optional flags then)
  "Run the query COMMAND with printing FLAGS, then call THEN.
See `coq-query-function' for the arguments."
  (unless coq-query-function
    (user-error "Coq: no backend to run queries in this buffer"))
  (funcall coq-query-function command flags then))

(defconst notation-print-kinds-table
  '(("Print Scope(s)" 0) ("Print Visibility" 1))
  "Enumerates the different kinds of notation information one can ask to Coq.")

(defun coq-PrintScope ()
  "Show information on notations.  Coq specific."
  (interactive)
  (let*
      ((mods
        (completing-read "Infos on notation (TAB to see list): "
                         notation-print-kinds-table))
       (s (read-string  "Name (empty for all): "))
       ;; (all (string-equal s ""))
       )
    (cond
     ((and (string-equal mods "Print Scope(s)") (string-equal s ""))
      (coq-run-query "Print Scopes"))
     (t
      (coq-run-query (format "%s %s" mods s))))))

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
  "Return a string made of punctuation chars found immediately before position POS."
  (let ((res nil)
        (currpos pos))
    (while (coq-is-symbol-or-punct (char-before currpos))
      (setq res (concat (char-to-string (char-before currpos)) res))
      (setq currpos (- currpos 1)))
    res))


(defun coq-grab-punctuation-right (pos)
  "Return a string made of punctuation chars found immediately after position POS."
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
  (or (coq-id-at-point)
      (let ((notation (coq-notation-at-position (point))))
        (if notation (concat "\"" notation "\"") ""))))

(defcustom coq-remap-mouse-1 nil
  "Whether Coq mode should remap mouse button 1 to Coq queries.

This overrides the default global binding of (control mouse-1) and
\(shift mouse-1) (buffers and faces menus).  Hence it is nil by
default."
  :type 'boolean
  :group 'coq)


;; On a mouse event, try to query the id at point clicked.
(defun coq-id-under-mouse-query (event)
  "Query the prover about the identifier or notation near mouse click EVENT.
This is mapped to control/shift mouse-1, unless coq-remap-mouse-1
is nil (which is the default).

More precisely:
- ctrl click issues a Print query
- shift issues a Check query
- ctrl+shift click issued a Check query
- adding meta (alt) to the modifiers perform the same query but with \"Set
  Printing All\" set
"
  (interactive "e")
  (save-selected-window
    (save-excursion
      (mouse-set-point event)
      (let* ((id (coq-id-at-point))
             (notat (coq-notation-at-position (point)))
             (modifs (event-modifiers event))
             (shft (member 'shift modifs))
             (ctrl (member 'control modifs))
             (mta (member 'meta modifs))
             (c (cond
                 ((and (not notat) (not id))
                  (error "Can't detect anything meaningful under mouse"))
                 ((and notat (not id)) "Locate")
                 ((and ctrl shft) "Check")
                 (shft "About")
                 (ctrl "Print")))
             ;; Notation need to be surrounded by ""
             (cmd (format (concat c " %s") (or id (concat "\"" notat "\"")))))
        ;; meta means: Set printing all for the command
        (coq-run-query cmd (and mta '("Set Printing All")))))))

(defun coq-guess-or-ask-for-string (s &optional dontguess)
  "Asks for a Coq identifier with message S.
If DONTGUESS is non nil, propose a default value as follows:

If region is active, propose its content as default value.

Otherwise propose identifier at point if any."
  (let* ((guess
          (cond
           (dontguess nil)
           ((use-region-p)
            (buffer-substring-no-properties (region-beginning) (region-end)))
           (t (coq-id-or-notation-at-point)))))
    (read-string
     (if guess (concat s " (default " guess "): ") (concat s ": "))
     nil 'proof-minibuffer-history guess)))


(defun coq-ask-do (ask do &optional dontguess postformatcmd wait flags)
  "Ask for an argument with prompt ASK and run the query DO on it.
The argument is guessed from point or region, unless DONTGUESS, see
`coq-guess-or-ask-for-string'.  If non-nil, POSTFORMATCMD formats the
argument.  WAIT may be a function, called once the answer is
displayed, any other non-nil value waits for the answer.  FLAGS are
printing flags for this query only, see `coq-query-function'."
  (let ((postform (or postformatcmd #'identity))
        (arg (coq-guess-or-ask-for-string ask dontguess)))
    (coq-run-query (concat do " " (funcall postform arg))
                   flags
                   (cond ((functionp wait) wait)
                         (wait #'ignore)))))


(defun coq-ask-do-set-unset (ask do setcmd _unsetcmd
                                 &optional dontguess postformatcmd _tescmd)
  "Ask for an argument with prompt ASK and run the query DO on it.
Run it with the printing flag SETCMD, for instance \"Set Printing All\",
for this query only: the backend undoes it, UNSETCMD is ignored.  See
`coq-ask-do' for DONTGUESS and POSTFORMATCMD."
  (coq-ask-do ask do dontguess postformatcmd nil (list setcmd)))


(defun coq-ask-do-show-implicits (ask do &optional dontguess postformatcmd)
  "Ask for an ident and print the corresponding term."
  (coq-ask-do-set-unset ask do
                        "Set Printing Implicit"
                        "Unset Printing Implicit"
                        dontguess postformatcmd
                        "Test Printing Implicit"))

(defun coq-ask-do-show-all (ask do &optional dontguess postformatcmd)
  "Ask for an ident and print the corresponding term."
  (coq-ask-do-set-unset ask do
                        "Set Printing All"
                        "Unset Printing All"
                        dontguess postformatcmd
                        "Test Printing All"))


(defun coq-SearchIsos ()
  "Search a term whose type is isomorphic to given type.
This is specific to `coq-mode'."
  (interactive)
  (coq-ask-do
   "SearchPattern (parenthesis mandatory), ex: (?X1 + _ = _ + ?X1)"
   "SearchPattern" nil))

(defun coq-SearchRewrite ()
  (interactive)
  (coq-ask-do "SearchRewrite" "SearchRewrite" nil))

(defun coq-Print (withprintingall)
  "Ask for an ident and print the corresponding term.
With flag Printing All if some prefix arg is given (\\[universal-argument])."
  (interactive "P")
  (if withprintingall
      (coq-ask-do-show-all "Print" "Print")
    (coq-ask-do "Print" "Print")))

(defun coq-Print-Ltac ()
  "Ask for an ident and print the corresponding Ltac term."
  (interactive)
  (coq-ask-do "Print Ltac" "Print Ltac"))

(defun coq-Print-with-implicits ()
  "Ask for an ident and print the corresponding term."
  (interactive)
  (coq-ask-do-show-implicits "Print" "Print"))

(defun coq-Print-with-all ()
  "Ask for an ident and print the corresponding term."
  (interactive)
  (coq-ask-do-show-all "Print" "Print"))

(defun coq-About (withprintingall)
  "Ask for an ident and print information on it."
  (interactive "P")
  (if withprintingall
      (coq-ask-do-show-all "About" "About")
    (coq-ask-do "About" "About")))

(defun coq-About-with-implicits ()
  "Ask for an ident and print information on it."
  (interactive)
  (coq-ask-do-show-implicits "About" "About"))

(defun coq-About-with-all ()
  "Ask for an ident and print information on it."
  (interactive)
  (coq-ask-do-show-all "About" "About"))


(defun coq-LocateConstant ()
  "Locate a constant."
  (interactive)
  (coq-ask-do "Locate" "Locate"))

(defun coq-LocateLibrary ()
  "Locate a library."
  (interactive)
  (coq-ask-do "Locate Library" "Locate Library"))

(defun coq-LocateNotation ()
  "Locate a notation.  Put it automatically into quotes.
This is specific to `coq-mode'."
  (interactive)
  (coq-ask-do
   "Locate notation (ex: \'exists\' _ , _)" "Locate"
   ))

(defun coq-Pwd ()
  "Display the current Coq working directory."
  (interactive)
  (coq-run-query "Pwd"))

(defun coq-Inspect ()
  (interactive)
  (coq-ask-do "Inspect how many objects back?" "Inspect" t))

(defun coq-PrintSection()
  (interactive)
  (coq-ask-do "Print Section" "Print Section" t))

(defun coq-Print-implicit ()
  "Ask for an ident and print the corresponding term."
  (interactive)
  (coq-ask-do "Print Implicit" "Print Implicit"))

(defun coq-Check (withprintingall)
  "Ask for a term and print its type.
With flag Printing All if some prefix arg is given (\\[universal-argument])."
  (interactive "P")
  (if withprintingall
      (coq-ask-do-show-all "Check" "Check")
    (coq-ask-do "Check" "Check")))

(defun coq-Check-show-implicits ()
  "Ask for a term and print its type."
  (interactive)
  (coq-ask-do-show-implicits "Check" "Check"))

(defun coq-Check-show-all ()
  "Ask for a term and print its type."
  (interactive)
  (coq-ask-do-show-all "Check" "Check"))

(defun coq-query (showall)
  "Ask for a query, with completion, and send to Coq."
  (interactive "P")
  (let ((q (coq-build-command-from-db coq-queries-commands-db "which Query?")))
    (coq-run-query (coq-remove-trailing-dot (string-trim-right q))
                   (and showall '("Set Printing All")))))

(defun coq-Search ()
  (interactive)
  (coq-ask-do
   ;; TODO: use [Add Search Blacklist "foo"] to exclude optionaly some patterns:
   ;;  "_ind" "_rec" "R_" "_equation"
   "Search (ex: \"eq_\" eq -bool)"
   "Search"
   nil nil #'coq-search-done))

(defun coq-search-done ()
  "Highlight the hypotheses cited by the answer of a Search, if wanted.
See `coq-highlight-hyps-cited-in-response'."
  (if (and coq-highlight-hyps-cited-in-response
           (fboundp 'coq-highlight-hyps-cited-in-response))
      (progn
        (coq-highlight-hyps-cited-in-response)
        (message "M-x coq-unhighlight-selected-hyps to remove highlighting, [Coq/Settings/Search Blacklist] to change blacklisting."))
    (message "[Coq/Settings/Search Blacklist] to change blacklisting.")))

;; Items of the menu Show, see coq-abbrev.el

(defun coq-PrintHint ()
  "Print the hints that apply to the current goal."
  (interactive)
  (coq-run-query "Print Hint"))

(defun coq-show-tree ()
  "Show the proof tree."
  (interactive)
  (coq-run-query "Show Tree"))

(defun coq-show-proof ()
  "Show the proof term built so far."
  (interactive)
  (coq-run-query "Show Proof"))

(defun coq-show-conjectures ()
  "Show the names of the current conjectures."
  (interactive)
  (coq-run-query "Show Conjectures"))

(defun coq-show-intros ()
  "Show the names that intros would give.
See `coq-insert-intros' to insert them."
  (interactive)
  (coq-run-query "Show Intros"))

(provide 'coq-queries)

;;; coq-queries.el ends here
