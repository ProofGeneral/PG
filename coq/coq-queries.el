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

(require 'cl-lib)                       ; `cl-pushnew'
(require 'subr-x)                       ; `string-trim-right'
(require 'coq-syntax)                   ; `coq-with-altered-syntax-table'
(require 'coq-db)                       ; `coq-build-command-from-db'
(require 'coq-goals)                    ; `coq-highlight-hyps-cited-in-response'
(require 'coq-indent)                   ; `coq-script-parse-cmdend-forward'

(defvar proof-minibuffer-history)
(defvar coq-one-command-per-line)
(declare-function coq-terminator-insert "coq-mode")

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

(defvar-local coq-query-result-function nil
  "Function running a query and returning its answer, set by each backend.
It is called with the argument COMMAND, the query without its final
dot, for instance \"Show Intros\".  It waits for the answer and returns
it as a string, without displaying it, or nil if Coq answers an error.")

(defun coq-query-result (command)
  "Run the query COMMAND and return its answer, nil on an error.
See `coq-query-result-function'."
  (unless coq-query-result-function
    (user-error "Coq: no backend to run queries in this buffer"))
  (funcall coq-query-result-function command))

(defcustom coq-query-flags
  '(("All" . "Set Printing All")
    ("Implicit" . "Set Printing Implicit")
    ("Coercions" . "Set Printing Coercions")
    ("Parentheses" . "Set Printing Parentheses")
    ("No Notations" . "Unset Printing Notations")
    ("Universes" . "Set Printing Universes"))
  "Printing flags that a query can use, by name.
With \\[universal-argument] \\[universal-argument], query commands ask
which ones to use, see `coq-query-flags-from-prefix'.  Each value is the
Coq command setting a boolean flag, for this query only."
  :type '(alist :key-type (string :tag "Name")
                :value-type (string :tag "Coq command"))
  :group 'coq)

(defun coq-query-flags-from-prefix (arg)
  "Return the printing flags of a query with prefix argument ARG.
None without prefix argument.  With \\[universal-argument] \\[universal-argument] or more,
ask which ones among `coq-query-flags'.  With any other prefix
argument, for instance \\[universal-argument], Set Printing All."
  (cond
   ((null arg) nil)
   ((and (consp arg) (>= (car arg) 16))
    (mapcar (lambda (name) (cdr (assoc name coq-query-flags)))
            (completing-read-multiple "Printing flags: " coq-query-flags
                                      nil t)))
   (t (list "Set Printing All"))))

(defun coq--query-prompt (prompt flags)
  "PROMPT for a query, mentioning FLAGS if any."
  (if flags (format "%s [%s]" prompt (string-join flags ", ")) prompt))

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

(defvar coq-mouse-query-keymaps nil
  "The keymaps where `coq-remap-mouse-1' binds the queries at mouse click.
See `coq-mouse-query-add-keymap'.")

(defconst coq--mouse-query-keys
  '([(control down-mouse-1)] [(shift down-mouse-1)]
    [(control shift down-mouse-1)] [(control meta down-mouse-1)]
    [(meta shift down-mouse-1)])
  "The keys of the queries at mouse click, see `coq-id-under-mouse-query'.")

(defconst coq--mouse-query-ignored-keys
  '([(control mouse-1)] [(shift mouse-1)] [(control shift mouse-1)]
    [(meta shift mouse-1)])
  "The up events of `coq--mouse-query-keys', ignored.")

(defcustom coq-remap-mouse-1 nil
  "Whether Coq mode should remap mouse button 1 to Coq queries.
See `coq-id-under-mouse-query'.  This is for the script buffer and, in
the backends that support it, the goals and response buffers.

This overrides the default global binding of (control mouse-1) and
\(shift mouse-1) (buffers and faces menus).  Hence it is nil by
default."
  :type 'boolean
  :set (lambda (symbol value)
         (set-default symbol value)
         (mapc #'coq--mouse-query-update coq-mouse-query-keymaps))
  :group 'coq)

(defun coq--mouse-query-update (map)
  "Bind or unbind the queries at mouse click in MAP.
Bind them if `coq-remap-mouse-1' is non-nil, unbind them otherwise."
  (dolist (key coq--mouse-query-keys)
    (define-key map key (and coq-remap-mouse-1 #'coq-id-under-mouse-query)))
  (dolist (key coq--mouse-query-ignored-keys)
    (define-key map key (and coq-remap-mouse-1 #'ignore))))

(defun coq-mouse-query-add-keymap (map)
  "Add MAP to `coq-mouse-query-keymaps' and update its bindings.
`coq-mode-map' is there, each backend adds the keymaps of its goals
and response buffers."
  (cl-pushnew map coq-mouse-query-keymaps :test #'eq)
  (coq--mouse-query-update map))


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
        (arg (coq-guess-or-ask-for-string (coq--query-prompt ask flags)
                                          dontguess)))
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

(defun coq-Print (arg)
  "Ask for an ident and print the corresponding term.
With \\[universal-argument], with Printing All.  With
\\[universal-argument] \\[universal-argument], ask which printing flags to use, see
`coq-query-flags-from-prefix'."
  (interactive "P")
  (coq-ask-do "Print" "Print" nil nil nil (coq-query-flags-from-prefix arg)))

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

(defun coq-About (arg)
  "Ask for an ident and print information on it.
With \\[universal-argument], with Printing All.  With
\\[universal-argument] \\[universal-argument], ask which printing flags to use, see
`coq-query-flags-from-prefix'."
  (interactive "P")
  (coq-ask-do "About" "About" nil nil nil (coq-query-flags-from-prefix arg)))

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

(defun coq-Check (arg)
  "Ask for a term and print its type.
With \\[universal-argument], with Printing All.  With
\\[universal-argument] \\[universal-argument], ask which printing flags to use, see
`coq-query-flags-from-prefix'."
  (interactive "P")
  (coq-ask-do "Check" "Check" nil nil nil (coq-query-flags-from-prefix arg)))

(defun coq-Check-show-implicits ()
  "Ask for a term and print its type."
  (interactive)
  (coq-ask-do-show-implicits "Check" "Check"))

(defun coq-Check-show-all ()
  "Ask for a term and print its type."
  (interactive)
  (coq-ask-do-show-all "Check" "Check"))

;;; Search blacklist

;; Default coq is only Private_ and _subproof
(defcustom coq-search-blacklist-string ; add this? \"_ind\" \"_rect\" \"_rec\"
 "\"Private_\" \"_subproof\""
  "Initial strings to blacklist in requests to Coq environment.
If you are setting this via Emacs customization menus, you should
restart Coq to see the effect.  To change blacklist during a Coq
development, please use \\[coq-change-search-blacklist-interactive]
instead (or menu: Coq/OTHER QUERIES/Search Blacklist)."
  :type 'string
  :group 'coq)

;; Coq has no command to overwrite the current blacklist, only to add or remove
;; particular strings. To provide the overwrite feature to the user we thus need
;; to know the current value of blacklist to first remove everything, then add
;; the new values.
;;; TODO: retrieve the value from coq itself and remove this variable?
(defvar coq-search-blacklist-current-string coq-search-blacklist-string
  "Current value of Coq Search Blacklist.
This may become desynchronized with Coq if you use the command
\"Add/Remove Search Blacklist\" in you script." )

(defvar-local coq-set-search-blacklist-function nil
  "Function replacing the search blacklist of Coq.
Set by each backend.  It is called with the arguments OLD and NEW, the
blacklists before and after, strings in the syntax of Add Search
Blacklist, like \"\\\"Private_\\\" \\\"_subproof\\\"\".")

(defvar coq-search-blacklist-history nil
  "History of `coq-change-search-blacklist-interactive'.")

(defun coq-change-search-blacklist (s)
  "Replace the search blacklist of Coq by S.
S is in the syntax of Add Search Blacklist.  See
`coq-set-search-blacklist-function'."
  (unless coq-set-search-blacklist-function
    (user-error "Coq: no backend to set the search blacklist in this buffer"))
  (funcall coq-set-search-blacklist-function
           coq-search-blacklist-current-string s)
  (setq coq-search-blacklist-current-string s))

(defun coq-change-search-blacklist-interactive ()
  "Ask for a new search blacklist and replace the one of Coq.
See `coq-change-search-blacklist'."
  (interactive)
  (coq-change-search-blacklist
   (read-string "Set search blacklist to: "
                coq-search-blacklist-current-string
                'coq-search-blacklist-history)))

;;; Printing flags for all the next goals and queries

(defvar-local coq-set-flag-function nil
  "Function setting a printing flag for the next goals and queries.
Set by each backend.  It is called with the argument COMMAND, setting or
unsetting a flag, without its final dot, for instance \"Set Printing
Parentheses\".  See `coq-query-flags' for a flag of one query only.")

(defun coq-set-flag (command)
  "Run COMMAND, setting a printing flag for the next goals and queries.
See `coq-set-flag-function'."
  (unless coq-set-flag-function
    (user-error "Coq: no backend to set printing flags in this buffer"))
  (funcall coq-set-flag-function command))

(defmacro coq-define-flag-command (name command)
  "Define the command NAME, running COMMAND with `coq-set-flag'."
  `(defun ,name ()
     ,(format "Run %s.\nFor the next goals and queries, see `coq-set-flag'."
              command)
     (interactive)
     (coq-set-flag ,command)))

(coq-define-flag-command coq-set-printing-implicit "Set Printing Implicit")
(coq-define-flag-command coq-unset-printing-implicit "Unset Printing Implicit")
(coq-define-flag-command coq-set-printing-all "Set Printing All")
(coq-define-flag-command coq-unset-printing-all "Unset Printing All")
(coq-define-flag-command coq-set-printing-parentheses "Set Printing Parentheses")
(coq-define-flag-command coq-unset-printing-parentheses "Unset Printing Parentheses")
(coq-define-flag-command coq-set-printing-notations "Set Printing Notations")
(coq-define-flag-command coq-unset-printing-notations "Unset Printing Notations")
(coq-define-flag-command coq-set-printing-synth "Set Printing Synth")
(coq-define-flag-command coq-unset-printing-synth "Unset Printing Synth")
(coq-define-flag-command coq-set-printing-coercions "Set Printing Coercions")
(coq-define-flag-command coq-unset-printing-coercions "Unset Printing Coercions")
(coq-define-flag-command coq-set-printing-compact-contexts "Set Printing Compact Contexts")
(coq-define-flag-command coq-unset-printing-compact-contexts "Unset Printing Compact Contexts")
(coq-define-flag-command coq-set-printing-unfocused "Set Printing Unfocused")
(coq-define-flag-command coq-unset-printing-unfocused "Unset Printing Unfocused")
(coq-define-flag-command coq-set-printing-universes "Set Printing Universes")
(coq-define-flag-command coq-unset-printing-universes "Unset Printing Universes")
(coq-define-flag-command coq-set-printing-wildcards "Set Printing Wildcard")
(coq-define-flag-command coq-unset-printing-wildcards "Unset Printing Wildcard")

;;; Insertion of "as" clauses

(defvar coq-keywords-accepting-as-regex (regexp-opt '("induction" "destruct" "inversion" "injection")))

;; destruct foo., where foo is a name.
(defvar coq-auto-as-hack-hyp-name-regex
  (concat "\\(" "induction\\|destruct" "\\)\\s-+\\(" coq-id-shy "\\)\\s-*\\.")
  "Regexp of commands needing a hack to generate correct \"as\" close.
tactics like destruct and induction reuse hypothesis names by
default, which makes the detection of new hypothesis incorrect.
the hack consists in adding the \"!\" modifier on the argument
destructed, so that it is left in the goal and the name cannot be
reused.  We also add a \"clear\" at the end of the tactic so that
the whole tactic behaves correctly.
Warning: this makes the error messages (and location) wrong.")

(defun coq-hack-cmd-for-infoH (s)
  "Return the tactic S hacked with infoH tactical."
  (cond
   ;; We cannot rebuild the sub-patterns from the final goal, so ';' is not
   ;; supported. Only single tactics like destruct.
   ((string-match ";" s) s) ;; composed tactic cannot be treated
   ((string-match coq-auto-as-hack-hyp-name-regex s)
    (concat "infoH " (match-string 1 s) " (" (match-string 2 s) ")."))
   ((string-match coq-keywords-accepting-as-regex s)
    (concat "infoH " s))
   (t (concat "infoH " s))))

;; Point supposed to be at the end of locked region, that is
;; (proof-assert-next-command-interactive) has just finished
(defun coq-tactic-already-has-an-as-close(s)
  "Return t if the last tactic of locked region contains an \"as\" close."
  (save-excursion (string-match "\\<as\\>" s)))

(defvar-local coq-processed-end-function nil
  "Function returning the end of the part of the script processed by Coq.
Set by each backend, called without argument in the script buffer.")

(defun coq-insert-as-in-next-command ()
  "Insert an \"as\" clause in the next command, from the names Coq gives.
The next command is the one after the processed part of the script, see
`coq-processed-end-function'.  It is run with the tactical infoH as a
query, see `coq-query-result', which leaves the state of Coq unchanged."
  (interactive)
  (unless coq-processed-end-function
    (user-error "Coq: no backend to run the next command in this buffer"))
  (save-excursion
    (goto-char (funcall coq-processed-end-function))
    (forward-char 1)
    (coq-find-real-start)
    (let* ((pt (point))
           (_ (coq-script-parse-cmdend-forward))
           (cmd (buffer-substring pt (point)))
           (newcmd (and (not (coq-tactic-already-has-an-as-close cmd))
                        (coq-hack-cmd-for-infoH cmd)))
           ;; not a composed tactic, see `coq-hack-cmd-for-infoH'
           (output (and newcmd
                        (string-prefix-p "infoH " newcmd)
                        (coq-query-result
                         (replace-regexp-in-string "\\.\\s-*\\'" "" newcmd))))
           (substr (and output
                        (string-match "<infoH>\\([^<]*\\)</infoH>" output)
                        (match-string 1 output))))
      ;; nothing if substr contains only spaces and |
      (when (and substr (string-match-p "[^|[:space:]]" substr))
        ;; TODO: look for eqn:XX and go before it.
        ;; Go just before the last "."
        (goto-char (funcall coq-processed-end-function))
        (coq-script-parse-cmdend-forward)
        (coq-script-parse-cmdend-backward)
        (insert (concat " as [" substr "]"))))))

(defvar-local coq-show-at-point-function nil
  "Function showing the state of Coq at point, set by each backend, or nil.
`coq-Show' calls it without argument.  It returns nil if it shows
nothing, for instance outside of the processed part: `coq-Show' asks
for a goal number then.")

(defun coq-Show (withprintingall)
  "Show the state at point, or ask for a number I and show the Ith goal.
The state at point is shown if the backend can, see
`coq-show-at-point-function'.  Otherwise the prefix argument gives the
printing flags, see `coq-query-flags-from-prefix':
\\[universal-argument] for Printing All, \\[universal-argument] \\[universal-argument] to choose them."
  (interactive "P")
  (unless (and coq-show-at-point-function
               (funcall coq-show-at-point-function))
    (coq-ask-do "Show goal number" "Show" t nil nil
                (coq-query-flags-from-prefix withprintingall))))

(defun coq-Show-with-implicits ()
  "Ask for a number i and show the ith goal."
  (interactive)
  (coq-ask-do-show-implicits "Show goal number" "Show" t))

(defun coq-Show-with-all ()
  "Ask for a number i and show the ith goal."
  (interactive)
  (coq-ask-do-show-all "Show goal number" "Show" t))

(defun coq-query (arg)
  "Ask for a query, with completion, and send to Coq.
With \\[universal-argument], with Printing All.  With
\\[universal-argument] \\[universal-argument], ask which printing flags to use, see
`coq-query-flags-from-prefix'."
  (interactive "P")
  (let* ((flags (coq-query-flags-from-prefix arg))
         (q (coq-build-command-from-db coq-queries-commands-db
                                       (coq--query-prompt "which Query?"
                                                          flags))))
    (coq-run-query (coq-remove-trailing-dot (string-trim-right q)) flags)))

(defun coq-Search (&optional arg)
  "Ask for a pattern and search the lemmas matching it.
With \\[universal-argument], with Printing All.  With
\\[universal-argument] \\[universal-argument], ask which printing flags to use, see
`coq-query-flags-from-prefix'."
  (interactive "P")
  (coq-ask-do
   ;; TODO: use [Add Search Blacklist "foo"] to exclude optionaly some patterns:
   ;;  "_ind" "_rec" "R_" "_equation"
   "Search (ex: \"eq_\" eq -bool)"
   "Search"
   nil nil #'coq-search-done (coq-query-flags-from-prefix arg)))

(defun coq-search-done ()
  "Highlight the hypotheses cited by the answer of a Search, if wanted.
See `coq-highlight-hyps-cited-in-response'."
  (if coq-highlight-hyps-cited-in-response
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

;; Insertions built from the answer of a query

(defun coq--format-intros (output)
  "Create an “intros” or ”move” form from the OUTPUT of “Show Intros”."
  (let* ((shints1 (replace-regexp-in-string "^[0-9] \\(?:sub\\)?goal\\(.\\|\n\\|\r\\)*"  "" output))
         (shints (replace-regexp-in-string "[\r\n ]*\\'" "" shints1)))
    (if (or (string= "" shints)
            (string-match coq-error-regexp shints))
        (error "Don't know what to intro")
      (save-excursion
        (if (re-search-backward "Require.*ssreflect" nil t)
            (format "move=> %s" shints)
          (format "intros %s" shints))))))

(defun coq-insert-intros ()
  "Insert an intros or move command with names given by Show Intros.
Based on idea mentioned in Coq reference manual."
  (interactive)
  (let* ((output (or (coq-query-result "Show Intros") "")))
    (indent-region (point)
                   (progn (insert (coq--format-intros output))
                          (save-excursion
                            ;; t if the backend does not define it
                            (insert (if (or (not (boundp 'coq-one-command-per-line))
                                            coq-one-command-per-line)
                                        "\n" " "))
                            (point))))
    ;; The electric terminator moves the point in all sorts of strange
    ;; ways, so we run it last
    (let ((last-command-event ?.)) ;; Insert a dot
      (coq-terminator-insert))))

(defun coq-replace-branch-with-yas-holes (s)
  "Replace occurrences of RE in by REPi in S. i is incremented at each replacement."
  (let ((res s) (i 2))
    (setq res (replace-regexp-in-string "\\`match #" "match $1" res))
    (while (string-match "=> *\n" res nil t)
      (setq res (replace-regexp-in-string ;; the regexp is made to match only
                                          ;; the fiorst occurrence
                 "\\(?1:=> *\n\\)\\(.\\|\n\\)*\\'"
                 (concat "=> $" (int-to-string i) "\n")
                 res nil nil 1))
      (setq i (+ 1 i)))
    res))

(defun coq-get-goal-names (show-existentials-output)
  "Return the list of goal names by parsing SHOW-EXISTENTIALS-OUTPUT.

Goals that are marked as \"only printing\" are ignored."
  (let ((goal-regex "Existential [0-9]+ =\\s-+\\?\\([^ ]+\\) :\\s-+\\[[^]]*]\\( (\\(?:shelved; \\)?only printing)\\)?")
        (pos 0)
        (matches '()))
    (while (string-match goal-regex show-existentials-output pos)
      (let ((goal-name (match-string 1 show-existentials-output))
            (only-printing (match-string 2 show-existentials-output)))
        (unless only-printing
          (push goal-name matches))
        (setq pos (match-end 0))))
    (nreverse matches)))

(defun coq-insert-named-goal-selectors ()
  "Insert named goal selectors for the open goals."
  (interactive)
  (let ((output (coq-query-result "Show Existentials")))
    (when output
      (let* ((goal-names (coq-get-goal-names output))
             ;; NOTE: Adding braces would be great, but it messes up indentation.
             (format-selector (lambda (name) (format "[%s]: #." name)))
             (goal-selectors (cl-mapcar format-selector goal-names))
             (snippet (string-join goal-selectors "\n")))
        (if (equal goal-selectors nil)
            (error "Couldn't find any named goals")
          (coq-insert-template snippet))))))

(defun coq-insert-match ()
  "Insert a match expression from a type name by Show Match.
Based on idea mentioned in Coq reference manual.
Also insert holes at insertion positions."
  (interactive)
  (let* ((cmd))
    (setq cmd (read-string "Build match for type: "))
    (let ((thematch (coq-query-result (concat "Show Match " cmd))))
      ;; nil on an error
      (when thematch
        (if (and coq-use-yasnippet (fboundp 'yas-expand-snippet))
            (yas-expand-snippet (coq-replace-branch-with-yas-holes thematch))
          (let ((start (point)))
            (insert thematch)
            (indent-region start (point) nil)
            (goto-char start)
            (search-forward "#")
            (delete-char -1)))))))

;;; Keys

(defun coq-queries-define-keys (map)
  "Bind the keys of the queries and of the display in MAP.
They are the default keys under C-c C-a of `coq-mode-map' for the
commands that make sense in a goals or response buffer too, see
`coq-all-buffers-commands'."
  ;; Queries, see `coq-query-function'.
  (define-key map (kbd "C-c C-a C-a") #'coq-Search)
  (define-key map (kbd "C-c C-a C-b") #'coq-About)
  (define-key map (kbd "C-c C-a C-c") #'coq-Check)
  (define-key map (kbd "C-c C-a C-o") #'coq-SearchIsos)
  (define-key map (kbd "C-c C-a C-p") #'coq-Print)
  (define-key map (kbd "C-c C-a C-q") #'coq-query)
  (define-key map (kbd "C-c C-a C-l") #'coq-LocateConstant)
  (define-key map (kbd "C-c C-a C-n") #'coq-LocateNotation)
  (define-key map (kbd "C-c C-a h") #'coq-PrintHint)
  (define-key map (kbd "C-c C-a C-s") #'coq-Show)
  ;; Display, see coq-goals.el.
  (define-key map (kbd "C-c C-a C-w") #'coq-ask-printing-width)
  (define-key map (kbd "C-c C-a r") #'coq-store-response-win)
  (define-key map (kbd "C-c C-a g") #'coq-store-goals-win)
  ;; Printing flags, see `coq-set-flag-function'.
  (define-key map (kbd "C-c C-a C-9") #'coq-set-printing-parentheses)
  (define-key map (kbd "C-c C-a C-0") #'coq-unset-printing-parentheses)
  (define-key map (kbd "C-c C-a N") #'coq-set-printing-notations)
  (define-key map (kbd "C-c C-a n") #'coq-unset-printing-notations))

(defvar coq-mode-map)

(defconst coq-all-buffers-commands
  '(;; queries
    coq-query coq-About coq-About-with-implicits coq-About-with-all
    coq-Check coq-Check-show-implicits coq-Check-show-all
    coq-Print coq-Print-with-implicits coq-Print-with-all coq-Print-implicit
    coq-Search coq-SearchRewrite coq-SearchIsos
    coq-change-search-blacklist-interactive
    coq-LocateConstant coq-LocateLibrary coq-LocateNotation
    coq-PrintHint coq-PrintScope coq-PrintSection coq-Inspect coq-Pwd
    coq-Show coq-Show-with-implicits coq-Show-with-all
    coq-show-tree coq-show-proof coq-show-conjectures
    ;; display
    coq-ask-printing-width coq-store-response-win coq-store-goals-win
    ;; printing options
    coq-set-printing-all coq-unset-printing-all
    coq-set-printing-implicit coq-unset-printing-implicit
    coq-set-printing-parentheses coq-unset-printing-parentheses
    coq-set-printing-notations coq-unset-printing-notations
    coq-set-printing-coercions coq-unset-printing-coercions
    coq-set-printing-compact-contexts coq-unset-printing-compact-contexts
    coq-set-printing-synth coq-unset-printing-synth
    coq-set-printing-universes coq-unset-printing-universes
    coq-set-printing-unfocused coq-unset-printing-unfocused
    coq-set-printing-wildcards coq-unset-printing-wildcards)
  "Commands of the script buffer also available in the goals and response buffers.
Their key bindings in `coq-mode-map', the default ones and those of the
user, are also in the goals and response buffers of all backends, see
`coq-all-buffers-keymap'.")

(defun coq-all-buffers-keymap ()
  "Return a new keymap for the goals and response buffers.
With the key bindings of `coq-mode-map' for `coq-all-buffers-commands',
the default ones and those of the user, but not those of its parent
\(in Proof General, `proof-mode-map').  Also C-c C-a C-r and C-c C-a C-g
for `coq-store-response-win' and `coq-store-goals-win': in the script
buffer, these keys are insertions.  Each backend makes it a parent of
the keymaps of its goals and response buffers, when it sets them up,
after the configuration of the user."
  (let ((map (make-sparse-keymap))
        (own (copy-keymap coq-mode-map)))
    (set-keymap-parent own nil)
    (dolist (command coq-all-buffers-commands)
      (dolist (key (where-is-internal command (list own) nil t))
        (unless (memq (aref key 0) '(menu-bar tool-bar))
          (define-key map key command))))
    (define-key map (kbd "C-c C-a C-r") #'coq-store-response-win)
    (define-key map (kbd "C-c C-a C-g") #'coq-store-goals-win)
    map))

;;; Menus, common to all Coq backends

(defconst coq-queries-menu-entries
  '(["Print..." coq-Print :help "With prefix arg (C-u): Set Printing All first"]
    ["Check..." coq-Check :help "With prefix arg (C-u): Set Printing All first"]
    ["About..." coq-About :help "With prefix arg (C-u): Set Printing All first"]
    ["Other..." coq-query]
    [ "Store Response" coq-store-response-win :help "Stores the content of response buffer in a dedicated buffer"]
    [ "Store Goal" coq-store-goals-win  :help "Stores the content of goals buffer in a dedicated buffer"])
  "Entries of the main queries, common to all Coq backends.
A list of menu items, not a submenu: Proof General puts them at the top
level of its Coq menu.")

(defconst coq-other-queries-menu
  '("OTHER QUERIES"
    ["Print Hint" coq-PrintHint t]
    ["Show ith Goal..." coq-Show t]
    ["Show ith Goal... (show implicits)" coq-Show-with-implicits t]
    ["Show ith Goal... (show all)" coq-Show-with-all t]
    ["Show Tree" coq-show-tree t]
    ["Show Proof" coq-show-proof t]
    ["Show Conjectures" coq-show-conjectures t];; maybe not so useful with editing in PG?
    ""
    ["Print..." coq-Print :help "With prefix arg (C-u): Set Printing All first"]
    ["Print... (show implicits)" coq-Print-with-implicits t]
    ["Print... (show all)" coq-Print-with-all t]
    ["Check..." coq-Check :help "With prefix arg (C-u): Set Printing All first"]
    ["Check (show implicits)..." coq-Check-show-implicits t]
    ["Check (show all)..." coq-Check-show-all t]
    ["About..." coq-About :help "With prefix arg (C-u): Set Printing All first"]
    ["About...(show implicits)" coq-About-with-implicits t]
    ["About...(show all)" coq-About-with-all t]
    ["Search..." coq-Search t]
    ["SearchRewrite..." coq-SearchRewrite t]
    ["SearchPattern..." coq-SearchIsos t]
    ["Search Blacklist..." coq-change-search-blacklist-interactive t]
    ["Locate constant..." coq-LocateConstant t]
    ["Locate Library..." coq-LocateLibrary t]
    ["Pwd" coq-Pwd t]
    ["Inspect..." coq-Inspect t]
    ["Print Section..." coq-PrintSection t]
    ""
    ["Locate notation..." coq-LocateNotation t]
    ["Print Implicit..." coq-Print-implicit t]
    ["Print Scope/Visibility..." coq-PrintScope t])
  "Menu of the other queries, common to all Coq backends.")

(defconst coq-options-menu
  '("OPTIONS"
    ["Set Printing All" coq-set-printing-all t]
    ["Unset Printing All" coq-unset-printing-all t]
    ["Set Printing Implicit" coq-set-printing-implicit t]
    ["Unset Printing Implicit" coq-unset-printing-implicit t]
    ["Set Printing Parentheses" coq-set-printing-parentheses t]
    ["Unset Printing Parentheses" coq-unset-printing-parentheses t]
    ["Set Printing Notations" coq-set-printing-notations t]
    ["Unset Printing Notations" coq-unset-printing-notations t]
    ["Set Printing Coercions" coq-set-printing-coercions t]
    ["Unset Printing Coercions" coq-unset-printing-coercions t]
    ["Set Printing Compact Contexts" coq-set-printing-compact-contexts t]
    ["Unset Printing Compact Contexts" coq-unset-printing-compact-contexts t]
    ["Set Printing Synth" coq-set-printing-synth t]
    ["Unset Printing Synth" coq-unset-printing-synth t]
    ["Set Printing Universes" coq-set-printing-universes t]
    ["Unset Printing Universes" coq-unset-printing-universes t]
    ["Set Printing Unfocused" coq-set-printing-unfocused t]
    ["Unset Printing Unfocused" coq-unset-printing-unfocused t]
    ["Set Printing Wildcards" coq-set-printing-wildcards t]
    ["Unset Printing Wildcards" coq-unset-printing-wildcards t]
    ["Set Printing Width" coq-ask-printing-width t])
  "Menu of the printing options, common to all Coq backends.
The flags go through `coq-set-flag-function', the width through
`coq-ask-printing-width-function'.")

(provide 'coq-queries)

;;; coq-queries.el ends here
